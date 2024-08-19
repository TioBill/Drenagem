(defun c:Dreno (/ 
                    ;; Functions
                    *error* verifica_layer calculateSecondPoint insertText putDefaultMLeaderSet putMiddleInfo drawLinePv

                    ;; Variables
                    INCLINACAO_DEFAULT firstPoint nivelPista profundidade textHeight
                    spaceFlag qFlag yDirection xDirection currentPv currentBL
                    grdata code data qFlag nextPoint line acadObj aDoc modelSpace
                    uFlag continue numeracaoBL numeracaoPV profundidadeBL
                    )
  

  (defun *error* (msg)
    (and uFlag (vla-endundomark aDoc))
    
    (or 
      (wcmatch (strcase msg t) "*break,*cancel*,*exit*") 
      (alert (strcat "ERROR: " msg "**"))
    )
  )
  
  (defun verifica_layer ()
    (entmake '((0 . "LAYER") (2 . "EMPRESAOSM_EIXOS") (70 . 0) (62 . 1) (6 . "CENTER2"))) 
    (entmake '((0 . "LAYER") (2 . "EMPRESAOSM_TEXTO_FINO") (70 . 0) (62 . 7) (6 . "Continuous")))
    (entmake '((0 . "LAYER") (2 . "EMPRESAOSM_INDILINHAS") (70 . 0) (62 . 11) (6 . "Continuous")))
  )
  
  (defun calculateSecondPoint (position textHeight / 
                               leftNRightRatio topNDownRatio xEndPos yEndPos zEndPos)
    (setq leftNRightRatio (* textHeight 4))
    (setq topNDownRatio (* textHeight 4)) ;; Landing Cap changes where the text ends
    
    
    (setq xEndPos (car position)
          yEndPos (cadr position)
          zEndPos (caddr position)
    )

    (if xDirection
      (setq xEndPos (- xEndPos leftNRightRatio))
      (setq xEndPos (+ xEndPos leftNRightRatio))
    )
    
    (if yDirection
      (setq yEndPos (+ yEndPos topNDownRatio))
      (setq yEndPos (- yEndPos topNDownRatio))
    )

    (list xEndPos yEndPos zEndPOs)
  )
  
  (defun insertText (currentPv modelSpace textHeight spaceFlag yDirection xDirection / previousLayer textString mleaderVLA)
    (setq previousLayer (getvar 'clayer))
    (setvar 'clayer "EMPRESAOSM_INDILINHAS")
    

    (setq textString
      (eval
        (strcat
          "{\\H0.888889x;\\C4;"
          (if spaceFlag
            "PV"
            "BL"
          )
          (rtos (car currentPv) 2 0) "\n"
          "\\H0.75x;\\C0;"
          "N.P. = " (rtos (cadr currentPv) 2 2) "\n"
          "N.F. = " (rtos (caddr currentPv) 2 2) "\n"
          "Profundidade = " (rtos (cadddr currentPv) 2 2)
          "}"
        )
      )
    )
    
    (command 
      "_mleader" 
      (nth 4 currentPv) ;; First Point
      (calculateSecondPoint (nth 4 currentPv) textHeight) 
      textString
    )

    (setq mleaderVLA (vlax-ename->vla-object (entlast)))
    
    (putDefaultMLeaderSet mleaderVLA textHeight)    

    (setvar 'clayer previousLayer)
  )

  (defun putDefaultMLeaderSet (mLeaderVLA textHeight)
    (vlax-put-property mleaderVLA 'TextHeight textHeight) ;; Change MLeader TextHeight
    (vlax-put-property mleaderVLA 'TextLeftAttachmentType 3) ;; Change Text left-Attachment 
    (vlax-put-property mleaderVLA 'TextRightAttachmentType 3) ;; Change Text left-Attachment 

    (if xDirection
      (vlax-put-property mleaderVLA 'LandingGap 2)
      (vlax-put-property mleaderVLA 'LandingGap 0)
    )
    (vlax-put-property mleaderVLA 'DogLegLength 0)
    (vlax-put-property mleaderVLA 'ArrowheadType 19) ;; None Arrow Type
  )

  (defun putMiddleInfo (line inclinacao textHeigth / startPointLine endPointLine angulo text)
    (setq 
      startPointLine (vlax-get line 'StartPoint)
      endPointLine  (vlax-get line 'EndPoint)
      angulo (angle (trans startPointLine 0 1) (trans endPointLine 0 1))
    )

    (setq previousLayer (getvar 'clayer))
    (setvar 'clayer "EMPRESAOSM_TEXTO_FINO")
    
    (setq text (vla-addmtext 
      modelSpace
      (vlax-3d-point 
        (/(+(car startPointLine)(car endPointLine))2)
        (/(+(cadr startPointLine)(cadr endPointLine))2)
        0 ;; No Z position
      )
      0 ;; Width 
      (strcat
        "L=" (rtos (vlax-get-property line 'Length) 2 2) "m\n"
        "I=" (rtos inclinacao 2 3) "m/m"
      )
    ))

    (vlax-put-property text 'Height textHeight)
    (vla-put-rotation text angulo) 
    (setvar 'clayer previousLayer) 
  )

  (defun drawLinePv (currentPv data inclinacao modelSpace textHeight
                     yDirection xDirection qFlag nivelPista
                     / previousLayer nivelFundo distanceLine declividade nextPv line)
    
    
    (if (and qFlag (< nivelPista (cadr currentPv)))
      (progn
        (setq nivelFundo (- nivelPista 1))
        
        (setq distanceLine (distance (nth 4 currentPv) data))
        (setq declividade (/ (abs (- nivelFundo (cadr currentPv))) distanceLine))
        
        (if (< declividade 0.005 )
          (setq nivelFundo (- (nth 2 currentPv) (* inclinacao distanceLine)))
        )
      )

      (setq nivelFundo 
           (- (caddr currentPv) (* (distance data (nth 4 currentPv)) inclinacao))
      ) ;; Nivel Fundo
    )
    
    (if qFlag
      (setq nextPv
        (list 
          (1+ (car currentPv))
          nivelPista
          nivelFundo
          (- nivelPista nivelFundo)
          data
        )
      )
      
      (setq nextPv
        (list
          (1+ (car currentPv)) ;; Numeracao
          (cadr currentPv)     ;; Nivel Terreno
          nivelFundo           ;; Nivel Fundo
          (- (cadr currentPv) nivelFundo) ;; Profundidade
          data ;; Posicao
        )
      )
    )
    
    
    (setq previousLayer (getvar 'clayer))
    (setvar 'clayer "EMPRESAOSM_EIXOS")

    ;; Draw Line
    (setq line 
      (vla-addline 
        modelSpace
        (vlax-3d-point (trans (nth 4 currentPv) 1 0))
        (vlax-3d-point (trans (nth 4 nextPv) 1 0))
      )
    )

    (setvar 'clayer previousLayer)
     
    (insertText nextPv modelSpace textHeight spaceFlag yDirection xDirection)

    (if qFlag
      (putMiddleInfo 
        line 
        (abs 
          (/ 
            (- nivelFundo (nth 2 currentPv)) 
            (distance 
              (nth 4 currentPv) 
              (nth 4 nextPv)
            )
          )
        ) 
        textHeight
      )
      
      (putMiddleInfo 
        line 
        inclinacao 
        textHeight
      )
    )
    
    nextPv
  )

  
  (setq
    acadObj (vlax-get-acad-object)
    aDoc (vla-get-activedocument acadObj)
    modelSpace (vla-get-modelspace aDoc)
  )

  (setvar 'cecolor "256")
  
  (setq uFlag (not (vla-startundomark aDoc)))
  

  (setvar 'cmdecho 0) ;; Desactivate cmdecho
  
  (setq INCLINACAO_DEFAULT 0.005)
  
  (verifica_layer)
  
  
  (setq firstPoint (getpoint "Selecione o ponto inicial: "))

  (setq firstPoint
    (list
      (car firstPoint)
      (cadr firstPoint)
      0
    )
  )
  
  (setq nivelPista (getreal "Digite o nivel de pista/terreno inicial:" )) 
  (setq profundidade (getreal "Nivel profundidade: "))
  (setq textHeight (getreal "Digite o tamanho do texto: "))
  (setq profundidadeBL 0.5) ;; Permitir usuario de escolher?

  
  (setq
    spaceFlag t
    qFlag nil

    ;; Position flags
    yDirection t
    xDirection nil

    numeracaoPV 1
    numeracaoBL 1
  )
  
  (prompt "Text Position:")
  (prompt 
    (eval
      (strcat
      "Direcao X = " (if xDirection "Left" "Right") "\n"
      "Direcao Y = " (if yDirection "Top" "Down") "\n"
      )
    )
  )
  
  
  (initget 1 "Yes No")
  (setq continue (getkword "Continuar de um ponto existente? (Yes or No) "))

  
  (if (eq continue "No")
    (progn
      (setq currentPv
        (list
          numeracaoPV
          nivelPista
          (- nivelPista profundidade)
          profundidade
          firstPoint
        )
      )

      (insertText currentPv modelSpace textHeight spaceFlag yDirection xDirection)
    )
      
    (progn
      (initget (+ 1 2))
      (setq numeracaoPV (getint "Digite numeracao do PV ATUAL: "))
      (setq numeracaoBL (1+ (getint "Digite numeracao do BL ATUAL: ")))

      (setq currentPv
        (list
          numeracaoPV
          nivelPista
          (- nivelPista profundidade)
          profundidade
          firstPoint
        )
      )
    )
  )

  (setq currentBL
    (list
      numeracaoBL
      nivelPista
      (- nivelPista profundidadeBL)
      profundidadeBL
      firstPoint
    )
  )

  (alert
    "
    MLeader Direction commands:\n
    [W] - Up/Cima
    [A] - Left/Esquerda
    [S] - Down/Baixo
    [D] - Right/Direita\n
    
    [Q] - Change Level/Mudar Nivel\n
    [SPACE] - Change Mode (PV-BL)/Trocar modo (PV-BL)\n
    [LEFT CLICK] - Draw line/Desenhar linha
    "
  )
  
  (while
    (progn
      (setq
        grdata (grread 't 15 0)
        code (car grdata)
        data (cadr grdata)
      )
      
      (cond
        ;; Botao SPACE pressionado
        ((and (= code 2) (= data 32))
          (setq spaceFlag (not spaceFlag))
          
          (if spaceFlag
            (prompt "Modo PV ativado!\n")
            (prompt "Modo BL ativado!\n")
          )

          t
        )

        ;; Setting MLeader direction 
        ((and (= code 2) (or (= data (ascii "W")) (= data (ascii "w"))))
          (setq yDirection t)  

         (prompt "Direcao Y: Top\n")

          t
        )
        ((and (= code 2) (or (= data (ascii "A")) (= data (ascii "a"))))
          (setq xDirection t)  

          (prompt "Direcao X: Left\n")
          t
        )
        ((and (= code 2) (or (= data (ascii "S")) (= data (ascii "s"))))
          (setq yDirection nil)  

          (prompt "Direcao Y: Down\n")
          t
        )
        ((and (= code 2) (or (= data (ascii "D")) (= data (ascii "d"))))
          (setq xDirection nil)  

          (prompt "Direcao X: Right\n")
          t
        
        )
        
        ;; Botão Q pressionado
        ((and (= code 2) (or (= data (ascii "Q")) (= data (ascii "q"))))
          (setq qFlag t)
         
          (setq nivelPista (getreal "Digite o nivel de pista/terreno:" )) 
         
          (setq currentBL
            (list
              (nth 0 currentBL)
              nivelPista
              (- nivelPista profundidade)
              (nth 3 currentBL)
              (nth 4 currentBL)
            )
          )

         t
        )
      
        ;; Botão Esquerdo do mouse pressionado
        ((and (= code 3) (listp data))

          ;; Remove Z-Direction
          (setq data
            (list
              (car data)
              (cadr data)
              0
            )
          )
         
          (if spaceFlag
            ;; PV MODE
            (setq currentPv 
                   (drawLinePv
                     currentPv 
                     data
                     INCLINACAO_DEFAULT 
                     modelSpace 
                     textHeight
                   
                     yDirection
                     xDirection

                      qFlag
                      nivelPista
                   )
            )
            
            ;; BL MODE
            (progn
              (setq currentBL (subst data (nth 4 currentBL) currentBL))
              
              (insertText currentBL modelSpace textHeight spaceFlag yDirection xDirection)
              (setq nextPoint (getpoint "Next point: "))

              ;; Remove Z-direction
              (setq nextPoint 
                (list
                  (car nextPoint)
                  (cadr nextPoint)
                  0
                )
              )
              
              (setvar 'clayer "EMPRESAOSM_EIXOS")
              ;; Draw Line

              (setq line 
                (vla-addline
                  modelSpace
                  (vlax-3d-point (trans (nth 4 currentBL) 1 0))
                  (vlax-3d-point (trans nextPoint 1 0))
                )
              )
              
              (putMiddleInfo 
                line 
                INCLINACAO_DEFAULT 
                textHeight
              )
              

              (setvar 'clayer "EMPRESAOSM_INDILINHAS")
              (command 
                "_mleader" 
                nextPoint ;; First Point
                (calculateSecondPoint nextPoint textHeight)
                (eval
                  (strcat
                    "{\\H0.888889x;\\C4;"
                    "BL" (rtos (nth 0 currentBL) 2 0) "\n"
                    "\\H0.75x;\\C0;"
                    "N.F: " (rtos 
                              (
                                - (caddr currentBL) 
                                  (
                                    * (vlax-get line 'Length)
                                     INCLINACAO_DEFAULT
                                  )
                              )
                              2 2
                            )
                    "}"
                  )
                )
              )
              
              (putDefaultMLeaderSet (vlax-ename->vla-object (entlast)) textHeight)    

              (setq currentBL
                (list 
                  (1+ (nth 0 currentBL))
                  (nth 1 currentBL)
                  (nth 2 currentBL)
                  (nth 3 currentBL)
                  (nth 4 currentBL)
                )
              )
            )
          )
         
         t
        )
        
        (t)
      )
    )
  ) 
)
  
(alert "Lisp carregada com sucesso! Digite \"DRENO\" para comecar.")
