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
;;;   Identifica i fori cilindrici in un solido 3D usando la
;;;   tecnica di esplosione temporanea, necessaria perché i fori
;;;   già sottratti tramite operazioni booleane (SUBTRACT) non
;;;   esistono più come sotto-entità separate ma sono fusi nella
;;;   geometria ACIS del solido.
;;;
;;;   Procedura:
;;;   1. Copia il solido (vla-Copy) per non modificare l'originale
;;;   2. Esplode iterativamente la copia (vla-Explode) fino ad
;;;      ottenere le curve primitive: solido → facce REGION →
;;;      curve (CIRCLE, ARC, LINE, ELLIPSE)
;;;   3. Filtra i CIRCLE tra le entità risultanti
;;;   4. Raggruppa i cerchi coassiali con lo stesso raggio per
;;;      identificare i fori unici:
;;;      - Foro passante: due cerchi coassiali (profondità reale
;;;        = distanza tra i centri)
;;;      - Foro cieco: un solo cerchio (profondità stimata dalla
;;;        dimensione del solido)
;;;   5. Cancella TUTTE le entità temporanee (anche in caso di
;;;      errore tramite gestore *error*)
;;;
;;;   Restituisce una lista di fori, ognuno nella forma:
;;;   (faccia pos-x pos-y diametro profondita)
;;; ------------------------------------------------------------
(defun analizza-fori-solido (nome-entita bbox-info
                              / oggetto-vla oggetto-copia entita-copia
                                coda-esplosione nuova-coda
                                lista-cerchi-info lista-fori
                                lun lar alt
                                x-min y-min z-min x-max y-max z-max
                                ent-corrente tipo-ent vla-ent
                                risultato-exp nuovo-ent
                                dati-c cx cy cz raggio normale-c
                                idx1 idx2 cerchi-usati
                                cx1 cy1 cz1 r1 nx1 ny1 nz1
                                cx2 cy2 cz2 r2 nx2 ny2 nz2
                                lunghezza-n dot-nn vx vy vz dist-cc
                                vx-n vy-n vz-n dot-vn
                                cerchio-gemello profondita-foro
                                nome-faccia pos-xy
                                livello-corrente)

  ; Inizializza le dimensioni del bounding box
  (setq lun   (nth 0 bbox-info)
        lar   (nth 1 bbox-info)
        alt   (nth 2 bbox-info)
        x-min (nth 3 bbox-info)
        y-min (nth 4 bbox-info)
        z-min (nth 5 bbox-info)
        x-max (nth 6 bbox-info)
        y-max (nth 7 bbox-info)
        z-max (nth 8 bbox-info))

  (setq lista-fori              '()
        lista-cerchi-info       '()
        *ANALISI-FORI-ENTI-TEMP* '())

  ; Installa il gestore errori per garantire la pulizia delle
  ; entità temporanee anche in caso di errore imprevisto.
  ; Le entità temporanee sono salvate nella variabile globale
  ; *ANALISI-FORI-ENTI-TEMP* accessibile dall'handler.
  (setq *ANALISI-FORI-ERR-ORIG* *error*)
  (defun *error* (msg)
    (foreach ent *ANALISI-FORI-ENTI-TEMP*
      (if (and ent (not (null (entget ent))))
        (entdel ent)
      )
    )
    (setq *ANALISI-FORI-ENTI-TEMP* '())
    (setq *error* *ANALISI-FORI-ERR-ORIG*)
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nErrore nell'analisi dei fori: " msg))
    )
  )

  ; --- Fase 1: Copia il solido per non modificare l'originale ---
  (setq oggetto-vla   (vlax-ename->vla-object nome-entita))
  (setq oggetto-copia (vla-Copy oggetto-vla))
  (setq entita-copia  (vlax-vla-object->ename oggetto-copia))
  (setq *ANALISI-FORI-ENTI-TEMP* (list entita-copia))

  ; --- Fase 2: Esplosione iterativa per estrarre le curve primitive ---
  ; Strategia a livelli: ogni iterazione del while processa un
  ; livello di esplosione. Le entità non esplodibili (curve
  ; primitive) rimangono in *ANALISI-FORI-ENTI-TEMP* per essere
  ; analizzate nella fase successiva.
  ; Il solido si decompone in: 3DSOLID → REGION (facce) → curve
  (setq coda-esplosione (list entita-copia)
        livello-corrente 0)

  (while (and coda-esplosione (< livello-corrente 5)) ; max 5 livelli: 3DSOLID→REGION→curve (5 è abbondante)
    (setq nuova-coda '())

    (foreach ent-corrente coda-esplosione
      (if (and ent-corrente (entget ent-corrente))
        (progn
          (setq tipo-ent (cdr (assoc 0 (entget ent-corrente))))

          ; Solo le entità composte vengono esplose ulteriormente;
          ; le curve primitive restano invariate in *ANALISI-FORI-ENTI-TEMP*
          (if (wcmatch tipo-ent "3DSOLID,REGION,SURFACE,BODY")
            (progn
              (setq vla-ent (vlax-ename->vla-object ent-corrente))
              (if vla-ent
                (progn
                  ; Tentativo di esplosione con cattura degli errori
                  (setq risultato-exp
                        (vl-catch-all-apply 'vla-Explode (list vla-ent)))
                  ; vla-Explode NON cancella l'originale:
                  ; ent-corrente è già in *ANALISI-FORI-ENTI-TEMP*
                  (if (not (vl-catch-all-error-p risultato-exp))
                    ; Esplosione riuscita: aggiungi le nuove entità
                    ; alla coda per il prossimo livello di esplosione
                    (if risultato-exp
                      (vlax-for nuovo-obj risultato-exp
                        (setq nuovo-ent (vlax-vla-object->ename nuovo-obj))
                        (if nuovo-ent
                          (progn
                            (setq nuova-coda
                                  (append nuova-coda (list nuovo-ent)))
                            (setq *ANALISI-FORI-ENTI-TEMP*
                                  (append *ANALISI-FORI-ENTI-TEMP*
                                          (list nuovo-ent)))
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )

    (setq coda-esplosione nuova-coda
          livello-corrente (1+ livello-corrente))
  )

  ; --- Fase 3: Raccoglie i cerchi tra tutte le entità temporanee ---
  ; I cerchi rappresentano i bordi circolari dei fori nel solido
  (foreach ent *ANALISI-FORI-ENTI-TEMP*
    (if (and ent (entget ent))
      (progn
        (setq tipo-ent (cdr (assoc 0 (entget ent))))
        (if (equal tipo-ent "CIRCLE")
          (progn
            (setq dati-c   (entget ent)
                  cx       (car   (cdr (assoc 10 dati-c)))
                  cy       (cadr  (cdr (assoc 10 dati-c)))
                  cz       (caddr (cdr (assoc 10 dati-c)))
                  raggio   (cdr (assoc 40 dati-c))
                  normale-c (cdr (assoc 210 dati-c)))
            (if (null normale-c)
              (setq normale-c '(0.0 0.0 1.0))
            )
            ; Salva: (cx cy cz raggio nx ny nz)
            (setq lista-cerchi-info
                  (append lista-cerchi-info
                          (list (list cx cy cz raggio
                                      (car normale-c)
                                      (cadr normale-c)
                                      (caddr normale-c)))))
          )
        )
      )
    )
  )

  ; --- Fase 4: Raggruppa i cerchi per identificare i fori ---
  ; Un foro è identificato da cerchi coassiali con lo stesso raggio:
  ; - Foro passante: due cerchi alle due estremità (profondità reale)
  ; - Foro cieco:    un solo cerchio sulla faccia di ingresso
  (setq cerchi-usati '()
        idx1          0)

  (foreach cerchio1 lista-cerchi-info
    (if (not (member idx1 cerchi-usati))
      (progn
        (setq cx1 (nth 0 cerchio1)
              cy1 (nth 1 cerchio1)
              cz1 (nth 2 cerchio1)
              r1  (nth 3 cerchio1)
              nx1 (nth 4 cerchio1)
              ny1 (nth 5 cerchio1)
              nz1 (nth 6 cerchio1))

        ; Normalizza il vettore normale del cerchio 1
        (setq lunghezza-n (sqrt (+ (* nx1 nx1) (* ny1 ny1) (* nz1 nz1))))
        (if (> lunghezza-n *TOLLERANZA*)
          (setq nx1 (/ nx1 lunghezza-n)
                ny1 (/ ny1 lunghezza-n)
                nz1 (/ nz1 lunghezza-n))
        )

        ; Cerca un cerchio gemello (altra estremità dello stesso foro)
        (setq cerchio-gemello nil
              profondita-foro 0.0
              idx2            0)

        (foreach cerchio2 lista-cerchi-info
          (if (and (not (= idx1 idx2))
                   (not (member idx2 cerchi-usati))
                   (null cerchio-gemello))
            (progn
              (setq cx2 (nth 0 cerchio2)
                    cy2 (nth 1 cerchio2)
                    cz2 (nth 2 cerchio2)
                    r2  (nth 3 cerchio2)
                    nx2 (nth 4 cerchio2)
                    ny2 (nth 5 cerchio2)
                    nz2 (nth 6 cerchio2))

              ; Normalizza il vettore normale del cerchio 2
              (setq lunghezza-n (sqrt (+ (* nx2 nx2) (* ny2 ny2) (* nz2 nz2))))
              (if (> lunghezza-n *TOLLERANZA*)
                (setq nx2 (/ nx2 lunghezza-n)
                      ny2 (/ ny2 lunghezza-n)
                      nz2 (/ nz2 lunghezza-n))
              )

              ; Condizione 1: stesso raggio (tolleranza relativa 0.1% del raggio)
              (if (< (abs (- r1 r2)) (* (max r1 r2) 0.001))
                (progn
                  ; Condizione 2: normali parallele → stesso asse del cilindro
                  ; (soglia 0.9 ≈ cos(26°): normali entro ~26° sono considerate parallele)
                  (setq dot-nn (+ (* nx1 nx2) (* ny1 ny2) (* nz1 nz2)))
                  (if (> (abs dot-nn) 0.9)
                    (progn
                      ; Condizione 3: vettore tra centri parallelo alla normale
                      ; (stessa soglia 0.9 ≈ cos(26°): verifica coassialità)
                      ; → i due cerchi sono coassiali (stesso foro)
                      (setq vx (- cx2 cx1)
                            vy (- cy2 cy1)
                            vz (- cz2 cz1))
                      (setq dist-cc (sqrt (+ (* vx vx) (* vy vy) (* vz vz))))
                      (if (> dist-cc *TOLLERANZA*)
                        (progn
                          (setq vx-n (/ vx dist-cc)
                                vy-n (/ vy dist-cc)
                                vz-n (/ vz dist-cc))
                          (setq dot-vn
                                (abs (+ (* vx-n nx1)
                                        (* vy-n ny1)
                                        (* vz-n nz1))))
                          (if (> dot-vn 0.9)
                            ; Foro passante: due cerchi coassiali trovati
                            (progn
                              (setq cerchio-gemello idx2
                                    profondita-foro dist-cc)
                              (setq cerchi-usati
                                    (append cerchi-usati (list idx2)))
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
          (setq idx2 (1+ idx2))
        )

        ; Marca il cerchio corrente come elaborato
        (setq cerchi-usati (append cerchi-usati (list idx1)))

        ; Per fori ciechi (nessun gemello) stima la profondità
        ; come dimensione del solido lungo l'asse del foro
        (if (null cerchio-gemello)
          (cond
            ((> (abs nz1) 0.9) (setq profondita-foro alt))
            ((> (abs ny1) 0.9) (setq profondita-foro lar))
            ((> (abs nx1) 0.9) (setq profondita-foro lun))
            (t                 (setq profondita-foro 0.0))
          )
        )

        ; Identifica la faccia di ingresso del foro tramite la normale
        (setq nome-faccia
              (identifica-faccia cx1 cy1 cz1
                                 x-min y-min z-min
                                 x-max y-max z-max
                                 (list nx1 ny1 nz1)))

        ; Calcola la posizione locale (2D) sulla faccia
        (setq pos-xy
              (calcola-posizione-su-faccia cx1 cy1 cz1 nome-faccia
                                           x-min y-min z-min
                                           x-max y-max z-max))

        ; Aggiunge il foro alla lista risultati
        (setq lista-fori
              (append lista-fori
                      (list (list nome-faccia
                                  (car pos-xy)
                                  (cadr pos-xy)
                                  (* 2.0 r1)
                                  profondita-foro))))
      )
    )
    (setq idx1 (1+ idx1))
  )

  ; --- Fase 5: Cancella TUTTE le entità temporanee ---
  (foreach ent *ANALISI-FORI-ENTI-TEMP*
    (if (and ent (not (null (entget ent))))
      (entdel ent)
    )
  )
  (setq *ANALISI-FORI-ENTI-TEMP* '())

  ; Ripristina il gestore errori originale
  (setq *error* *ANALISI-FORI-ERR-ORIG*)

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
