;;; ============================================================
;;; analizza3d.lsp
;;; Analizzatore di solidi 3D per AutoCAD
;;;
;;; Descrizione:
;;;   Questo programma AutoLISP permette di selezionare uno o
;;;   più solidi 3D (3DSOLID) nel disegno AutoCAD, calcola le
;;;   dimensioni di ciascun pezzo (Lunghezza, Larghezza, Altezza)
;;;   tramite il bounding box, individua i fori cilindrici
;;;   presenti sulle facce e salva tutte le informazioni in un
;;;   file di testo (.txt).
;;;
;;; Utilizzo:
;;;   Caricare il file con APPLOAD, poi eseguire il comando:
;;;   ANALIZZA3D
;;;
;;; File di output:
;;;   C:\Users\federico.piazzalunga\Desktop\TESTpy\analisi_pezzo.txt
;;; ============================================================

(vl-load-com)

;;; ------------------------------------------------------------
;;; Costanti globali
;;; ------------------------------------------------------------

; Percorso di salvataggio del file di output
(setq *PERCORSO-OUTPUT* "C:\\Users\\federico.piazzalunga\\Desktop\\TESTpy\\analisi_pezzo.txt")

; Tolleranza per i confronti geometrici (mm)
(setq *TOLLERANZA* 0.001)

;;; ------------------------------------------------------------
;;; Funzione di utilità: arrotonda un numero a N decimali
;;; ------------------------------------------------------------
(defun arrotonda (valore decimali / fattore)
  (setq fattore (expt 10.0 decimali))
  (/ (float (fix (+ (* valore fattore) (if (> valore 0) 0.5 -0.5)))) fattore)
)

;;; ------------------------------------------------------------
;;; Funzione: ottieni-dimensioni-bbox
;;;   Calcola le dimensioni del bounding box di un solido VLA.
;;;   Restituisce una lista: (lunghezza larghezza altezza
;;;                           pt-min-x pt-min-y pt-min-z
;;;                           pt-max-x pt-max-y pt-max-z)
;;; ------------------------------------------------------------
(defun ottieni-dimensioni-bbox (oggetto-vla / pt-min pt-max
                                               x-min y-min z-min
                                               x-max y-max z-max
                                               lun lar alt)
  (vla-GetBoundingBox oggetto-vla 'pt-min 'pt-max)
  (setq pt-min (vlax-safearray->list pt-min)
        pt-max (vlax-safearray->list pt-max))
  (setq x-min (car   pt-min)
        y-min (cadr  pt-min)
        z-min (caddr pt-min)
        x-max (car   pt-max)
        y-max (cadr  pt-max)
        z-max (caddr pt-max))
  (setq lun (abs (- x-max x-min))
        lar (abs (- y-max y-min))
        alt (abs (- z-max z-min)))
  (list lun lar alt
        x-min y-min z-min
        x-max y-max z-max)
)

;;; ------------------------------------------------------------
;;; Funzione: identifica-faccia
;;;   Data una coordinata Z del centro del cerchio (foro) e il
;;;   bounding box del solido, identifica su quale faccia si
;;;   trova il foro.
;;;   Restituisce una stringa tra:
;;;   "Alto", "Basso", "Fronte", "Retro", "Sinistra", "Destra"
;;;
;;;   La logica è:
;;;   - Se il centro è vicino a z-max -> faccia "Alto"
;;;   - Se il centro è vicino a z-min -> faccia "Basso"
;;;   - Se il centro è vicino a y-max -> faccia "Retro"
;;;   - Se il centro è vicino a y-min -> faccia "Fronte"
;;;   - Se il centro è vicino a x-max -> faccia "Destra"
;;;   - Se il centro è vicino a x-min -> faccia "Sinistra"
;;; ------------------------------------------------------------
(defun identifica-faccia (cx cy cz
                           x-min y-min z-min
                           x-max y-max z-max
                           normale / tol nome-faccia)
  (setq tol *TOLLERANZA*)

  ; Se il vettore normale della faccia è disponibile, lo usiamo
  ; per una determinazione più precisa
  (cond
    ; Faccia superiore (normale verso +Z)
    ((and normale
          (> (caddr normale) 0.5))
     (setq nome-faccia "Alto"))

    ; Faccia inferiore (normale verso -Z)
    ((and normale
          (< (caddr normale) -0.5))
     (setq nome-faccia "Basso"))

    ; Faccia posteriore (normale verso +Y)
    ((and normale
          (> (cadr normale) 0.5))
     (setq nome-faccia "Retro"))

    ; Faccia frontale (normale verso -Y)
    ((and normale
          (< (cadr normale) -0.5))
     (setq nome-faccia "Fronte"))

    ; Faccia destra (normale verso +X)
    ((and normale
          (> (car normale) 0.5))
     (setq nome-faccia "Destra"))

    ; Faccia sinistra (normale verso -X)
    ((and normale
          (< (car normale) -0.5))
     (setq nome-faccia "Sinistra"))

    ; Fallback: determinazione in base alla posizione del centro
    (t
     (cond
       ; Vicino alla faccia superiore
       ((< (abs (- cz z-max)) tol)
        (setq nome-faccia "Alto"))
       ; Vicino alla faccia inferiore
       ((< (abs (- cz z-min)) tol)
        (setq nome-faccia "Basso"))
       ; Vicino alla faccia posteriore
       ((< (abs (- cy y-max)) tol)
        (setq nome-faccia "Retro"))
       ; Vicino alla faccia frontale
       ((< (abs (- cy y-min)) tol)
        (setq nome-faccia "Fronte"))
       ; Vicino alla faccia destra
       ((< (abs (- cx x-max)) tol)
        (setq nome-faccia "Destra"))
       ; Vicino alla faccia sinistra
       ((< (abs (- cx x-min)) tol)
        (setq nome-faccia "Sinistra"))
       ; Caso non determinabile
       (t
        (setq nome-faccia "Sconosciuta"))
     )
    )
  )
  nome-faccia
)

;;; ------------------------------------------------------------
;;; Funzione: calcola-posizione-su-faccia
;;;   Converte le coordinate 3D globali del centro di un foro
;;;   in coordinate 2D locali (X, Y) rispetto alla faccia.
;;; ------------------------------------------------------------
(defun calcola-posizione-su-faccia (cx cy cz nome-faccia
                                     x-min y-min z-min
                                     x-max y-max z-max
                                     / px py)
  (cond
    ((equal nome-faccia "Alto")
     ; Sul piano XY a z-max: X = distanza da x-min, Y = distanza da y-min
     (setq px (- cx x-min)
           py (- cy y-min)))

    ((equal nome-faccia "Basso")
     ; Sul piano XY a z-min: X = distanza da x-min, Y = distanza da y-min
     (setq px (- cx x-min)
           py (- cy y-min)))

    ((equal nome-faccia "Fronte")
     ; Sul piano XZ a y-min: X = distanza da x-min, Y = distanza da z-min
     (setq px (- cx x-min)
           py (- cz z-min)))

    ((equal nome-faccia "Retro")
     ; Sul piano XZ a y-max: X = distanza da x-max, Y = distanza da z-min
     (setq px (- x-max cx)
           py (- cz z-min)))

    ((equal nome-faccia "Sinistra")
     ; Sul piano YZ a x-min: X = distanza da y-min, Y = distanza da z-min
     (setq px (- cy y-min)
           py (- cz z-min)))

    ((equal nome-faccia "Destra")
     ; Sul piano YZ a x-max: X = distanza da y-max, Y = distanza da z-min
     (setq px (- y-max cy)
           py (- cz z-min)))

    (t
     (setq px cx py cy))
  )
  (list px py)
)

;;; ------------------------------------------------------------
;;; Funzione: formatta-data
;;;   Restituisce la data e ora corrente come stringa leggibile
;;;   nel formato DD/MM/YYYY HH:MM:SS.
;;; ------------------------------------------------------------
(defun formatta-data (/ cdate-val parte-int parte-dec
                         anno mese giorno
                         ore minuti secondi tempo-val)
  ; getvar "CDATE" restituisce YYYYMMDD.HHMMSSmmm come numero reale
  (setq cdate-val (getvar "CDATE")
        parte-int (fix cdate-val)
        parte-dec (- cdate-val parte-int))

  ; Estrai anno, mese, giorno dalla parte intera (YYYYMMDD)
  (setq anno   (fix (/ parte-int 10000))
        mese   (fix (/ (rem parte-int 10000) 100))
        giorno (rem parte-int 100))

  ; Estrai ore, minuti, secondi dalla parte decimale (HHMMSSmmm -> HH.MM.SS)
  (setq tempo-val (* parte-dec 1000000.0))
  (setq ore     (fix (/ tempo-val 10000.0))
        minuti  (fix (/ (rem (fix tempo-val) 10000) 100))
        secondi (rem (fix tempo-val) 100))

  ; Formatta come DD/MM/YYYY HH:MM:SS
  (strcat (if (< giorno 10) (strcat "0" (itoa giorno)) (itoa giorno)) "/"
          (if (< mese   10) (strcat "0" (itoa mese))   (itoa mese))   "/"
          (itoa anno) " "
          (if (< ore     10) (strcat "0" (itoa ore))     (itoa ore))     ":"
          (if (< minuti  10) (strcat "0" (itoa minuti))  (itoa minuti))  ":"
          (if (< secondi 10) (strcat "0" (itoa secondi)) (itoa secondi)))
)

;;; ------------------------------------------------------------
;;; Funzione: analizza-fori-solido
;;;   Analizza le sotto-entità di un solido 3D per trovare i
;;;   fori cilindrici.
;;;   Restituisce una lista di fori, ognuno nella forma:
;;;   (faccia pos-x pos-y diametro profondita)
;;; ------------------------------------------------------------
(defun analizza-fori-solido (nome-entita bbox-info
                              / entita-dati tipo-entita
                                ultima-entita sotto-entita
                                dati-sottoentita
                                lista-fori lista-cerchi
                                cx cy cz raggio diametro
                                nome-faccia pos-xy profondita
                                x-min y-min z-min
                                x-max y-max z-max
                                lun lar alt)

  (setq lun    (nth 0 bbox-info)
        lar    (nth 1 bbox-info)
        alt    (nth 2 bbox-info)
        x-min  (nth 3 bbox-info)
        y-min  (nth 4 bbox-info)
        z-min  (nth 5 bbox-info)
        x-max  (nth 6 bbox-info)
        y-max  (nth 7 bbox-info)
        z-max  (nth 8 bbox-info))

  (setq lista-fori   '()
        lista-cerchi '())

  ; Scansiona le sotto-entità del solido cercando cerchi (fori).
  ; Le sotto-entità di un 3DSOLID con ACIS includono le facce,
  ; i bordi e i vertici. I fori cilindrici appaiono come bordi
  ; circolari (cerchi) sulle facce.
  ; Si usa entnext in modo corretto: ogni iterazione riceve
  ; l'ultima entità processata per ottenere la successiva.
  (setq ultima-entita nome-entita)
  (while (setq sotto-entita (entnext ultima-entita))
    ; Verifica se la sotto-entità è un cerchio
    (setq dati-sottoentita (entget sotto-entita))
    (setq tipo-entita (cdr (assoc 0 dati-sottoentita)))

    (if (wcmatch tipo-entita "CIRCLE,ARC")
      (setq lista-cerchi (append lista-cerchi (list sotto-entita)))
    )

    ; Avanza alla sotto-entità successiva
    (setq ultima-entita sotto-entita)
  )

  ; Per ogni cerchio trovato, crea un record foro
  (foreach ent-cerchio lista-cerchi
    (setq dati-sottoentita (entget ent-cerchio))
    (setq tipo-entita (cdr (assoc 0 dati-sottoentita)))

    (if (equal tipo-entita "CIRCLE")
      (progn
        ; Centro del cerchio
        (setq cx (car   (cdr (assoc 10 dati-sottoentita)))
              cy (cadr  (cdr (assoc 10 dati-sottoentita)))
              cz (caddr (cdr (assoc 10 dati-sottoentita))))
        ; Raggio e diametro
        (setq raggio   (cdr (assoc 40 dati-sottoentita))
              diametro (* 2.0 raggio))

        ; Vettore normale del piano del cerchio (gruppo 210)
        (setq normale-cerchio (cdr (assoc 210 dati-sottoentita)))
        (if (null normale-cerchio)
          (setq normale-cerchio '(0.0 0.0 1.0))
        )

        ; Identifica la faccia su cui si trova il foro
        (setq nome-faccia
              (identifica-faccia cx cy cz
                                  x-min y-min z-min
                                  x-max y-max z-max
                                  normale-cerchio))

        ; Calcola posizione locale sulla faccia
        (setq pos-xy
              (calcola-posizione-su-faccia cx cy cz nome-faccia
                                            x-min y-min z-min
                                            x-max y-max z-max))

        ; Stima della profondità del foro.
        ; NOTA: senza accesso diretto alla geometria ACIS, la profondità
        ; viene stimata come la dimensione massima del solido lungo l'asse
        ; perpendicolare alla faccia. Questo valore rappresenta la profondità
        ; massima possibile (foro passante). Per fori ciechi o parziali
        ; il valore reale sarà inferiore.
        (cond
          ((or (equal nome-faccia "Alto") (equal nome-faccia "Basso"))
           (setq profondita alt))
          ((or (equal nome-faccia "Fronte") (equal nome-faccia "Retro"))
           (setq profondita lar))
          ((or (equal nome-faccia "Sinistra") (equal nome-faccia "Destra"))
           (setq profondita lun))
          (t
           (setq profondita 0.0))
        )

        ; Aggiunge il foro alla lista
        (setq lista-fori
              (append lista-fori
                      (list (list nome-faccia
                                  (car pos-xy)
                                  (cadr pos-xy)
                                  diametro
                                  profondita))))
      )
    )
  )

  lista-fori
)

;;; ------------------------------------------------------------
;;; Funzione: scrivi-report
;;;   Scrive il report di analisi nel file di testo di output.
;;; ------------------------------------------------------------
(defun scrivi-report (lista-risultati / fh percorso
                                         risultato nome-id bbox-info
                                         lun lar alt lista-fori
                                         numero-foro foro
                                         faccia px py diametro prof)

  (setq percorso *PERCORSO-OUTPUT*)

  ; Apri il file in modalità scrittura
  (setq fh (open percorso "w"))

  (if (null fh)
    ; Impossibile aprire il file: notifica l'utente e termina la funzione
    (progn
      (alert (strcat "ERRORE: impossibile aprire il file di output:\n"
                     percorso
                     "\nVerificare che la cartella esista."))
    )
    ; File aperto con successo: scrivi il report
    (progn
      ; Intestazione del file
      (write-line "=== ANALISI PEZZI 3D ===" fh)
      (write-line (strcat "Data/ora analisi: " (formatta-data)) fh)
      (write-line (strcat "Numero pezzi analizzati: " (itoa (length lista-risultati))) fh)
      (write-line "" fh)

      ; Scrivi i dati per ogni pezzo
      (setq numero-pezzo 0)
      (foreach risultato lista-risultati
        (setq numero-pezzo  (1+ numero-pezzo)
              nome-id       (nth 0 risultato)
              bbox-info     (nth 1 risultato)
              lista-fori    (nth 2 risultato))

        (setq lun (arrotonda (nth 0 bbox-info) 3)
              lar (arrotonda (nth 1 bbox-info) 3)
              alt (arrotonda (nth 2 bbox-info) 3))

        (write-line (strcat "========================================") fh)
        (write-line (strcat "Pezzo " (itoa numero-pezzo) ": " nome-id) fh)
        (write-line (strcat "Dimensioni: L=" (rtos lun 2 3)
                                      " W=" (rtos lar 2 3)
                                      " H=" (rtos alt 2 3)) fh)
        (write-line "" fh)

        (if (null lista-fori)
          (write-line "  Nessun foro rilevato." fh)
          (progn
            (write-line "--- FORI RILEVATI ---" fh)
            (setq numero-foro 0)
            (foreach foro lista-fori
              (setq numero-foro (1+ numero-foro)
                    faccia      (nth 0 foro)
                    px          (arrotonda (nth 1 foro) 3)
                    py          (arrotonda (nth 2 foro) 3)
                    diametro    (arrotonda (nth 3 foro) 3)
                    prof        (arrotonda (nth 4 foro) 3))

              (write-line (strcat "Foro " (itoa numero-foro) ":") fh)
              (write-line (strcat "  Faccia: "      faccia) fh)
              (write-line (strcat "  Posizione X: " (rtos px 2 3)) fh)
              (write-line (strcat "  Posizione Y: " (rtos py 2 3)) fh)
              (write-line (strcat "  Diametro: "    (rtos diametro 2 3)) fh)
              (write-line (strcat "  Profondità: "  (rtos prof 2 3)) fh)
              (write-line "" fh)
            )
          )
        )
        (write-line "" fh)
      )

      (write-line "=== FINE REPORT ===" fh)
      (close fh)

      (princ (strcat "\nReport salvato in: " percorso))
    )
  )
)

;;; ------------------------------------------------------------
;;; Funzione principale: c:ANALIZZA3D
;;;   Comando AutoCAD lanciabile dalla linea di comando.
;;; ------------------------------------------------------------
(defun c:ANALIZZA3D (/ selezione numero-entita i nome-entita
                        entita-dati tipo-entita
                        oggetto-vla bbox-info
                        lista-fori lista-risultati
                        nome-id contatore-solidi)

  (princ "\n=== ANALIZZATORE PEZZI 3D ===")
  (princ "\nSelezionare i solidi 3D da analizzare...")

  ; Richiesta di selezione all'utente (solo entità 3DSOLID)
  (setq selezione (ssget '((0 . "3DSOLID"))))

  (if (null selezione)
    (progn
      (princ "\nNessun solido 3D selezionato. Operazione annullata.")
      (princ)
    )
    (progn
      (setq numero-entita    (sslength selezione)
            lista-risultati  '()
            contatore-solidi 0)

      (princ (strcat "\n" (itoa numero-entita) " solido/i selezionato/i. Analisi in corso..."))

      ; Ciclo su ogni entità selezionata
      (setq i 0)
      (while (< i numero-entita)
        (setq nome-entita  (ssname selezione i)
              entita-dati  (entget nome-entita)
              tipo-entita  (cdr (assoc 0 entita-dati)))

        ; Verifica che sia un 3DSOLID
        (if (equal tipo-entita "3DSOLID")
          (progn
            (setq contatore-solidi (1+ contatore-solidi))

            ; ID del pezzo: usa il nome handle AutoCAD
            (setq nome-id (strcat "Solido_" (cdr (assoc 5 entita-dati))))

            (princ (strcat "\n  Analisi " nome-id " ..."))

            ; Ottieni il bounding box tramite VLA
            (setq oggetto-vla (vlax-ename->vla-object nome-entita))
            (setq bbox-info   (ottieni-dimensioni-bbox oggetto-vla))

            ; Analizza i fori del solido
            (setq lista-fori (analizza-fori-solido nome-entita bbox-info))

            (princ (strcat " OK ("
                           (itoa (length lista-fori))
                           " foro/i rilevato/i)"))

            ; Aggiungi i risultati alla lista generale
            (setq lista-risultati
                  (append lista-risultati
                          (list (list nome-id bbox-info lista-fori))))
          )
        )

        (setq i (1+ i))
      )

      ; Scrivi il report
      (princ (strcat "\n\nAnalisi completata: "
                     (itoa contatore-solidi)
                     " solido/i elaborato/i."))
      (princ "\nGenerazione del report...")

      (scrivi-report lista-risultati)

      (princ "\nOperazione completata con successo.")
      (princ)
    )
  )
)

;;; ------------------------------------------------------------
;;; Messaggio di caricamento
;;; ------------------------------------------------------------
(princ "\nanalizza3d.lsp caricato correttamente.")
(princ "\nDigitare ANALIZZA3D per avviare l'analisi dei pezzi 3D.")
(princ)
