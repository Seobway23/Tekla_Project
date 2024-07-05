(defun c:GS (/ ss num i ent entData pt1 pt2 length lineList totalLength averageLength)
  ;; ���� �������� GRB_CENTER ���̾ ���ϴ� ��� ���� ����
  (setq ss (ssget 'C '((8 . "GRB_CENTER") (0 . "LINE"))))
  
  (if ss
    (progn
      ;; �ʱ�ȭ
      (setq num (sslength ss))
      (setq i 0)
      (setq totalLength 0.0)
      (setq lineList '())

      ;; ���� ������ �� ���� ��ȸ�ϸ� ���̸� ���
      (while (< i num)
        (setq ent (ssname ss i))
        (setq entData (entget ent))
        (setq pt1 (cdr (assoc 10 entData)))
        (setq pt2 (cdr (assoc 11 entData)))
        (setq length (distance pt1 pt2))

        ;; ���̸� ����Ʈ�� �߰��ϰ� �� ���̿� ����
        (setq lineList (cons length lineList))
        (setq totalLength (+ totalLength length))

        (setq i (1+ i)))

      ;; ��� ���̸� ���
      (setq averageLength (/ totalLength num))

      ;; ��� ���
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
