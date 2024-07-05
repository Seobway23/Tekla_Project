(defun c:GS (/ ss num i ent entData pt1 pt2 length lineList totalLength averageLength)
  ;; 선택 영역에서 GRB_CENTER 레이어에 속하는 모든 선을 선택
  (setq ss (ssget 'C '((8 . "GRB_CENTER") (0 . "LINE"))))
  
  (if ss
    (progn
      ;; 초기화
      (setq num (sslength ss))
      (setq i 0)
      (setq totalLength 0.0)
      (setq lineList '())

      ;; 선택 집합의 각 선을 순회하며 길이를 계산
      (while (< i num)
        (setq ent (ssname ss i))
        (setq entData (entget ent))
        (setq pt1 (cdr (assoc 10 entData)))
        (setq pt2 (cdr (assoc 11 entData)))
        (setq length (distance pt1 pt2))

        ;; 길이를 리스트에 추가하고 총 길이에 더함
        (setq lineList (cons length lineList))
        (setq totalLength (+ totalLength length))

        (setq i (1+ i)))

      ;; 평균 길이를 계산
      (setq averageLength (/ totalLength num))

      ;; 결과 출력
      (princ (strcat "\nTotal Length: " (rtos totalLength 2 2)))
      (princ (strcat "\nAverage Length: " (rtos averageLength 2 2)))
      (princ (strcat "\nNumber of Lines: " (itoa num)))
    )
    (princ "\nNo lines found on the GRB_CENTER layer in the selected area.")
  )

  (princ)
)

(princ "\nLISP command loaded. Use 'AnalyzeGRBCenterLines' command to analyze lines.")
(princ)
