(defun c:LinesToPolyline ( / ss entData startPoints endPoints tolerance)
  (setq ss (ssget '((0 . "LINE"))))  ; ���θ� ����
  (setq tolerance 0.01)  ; ���� ���� ����
  (setq startPoints '())
  (setq endPoints '())

  (if ss
    (progn
      (repeat (sslength ss)
        (setq entData (entget (ssname ss 0)))  ; ���õ� ù ��° ��ü�� ��ƼƼ ������ ���
        (setq startPoints (cons (cdr (assoc 10 entData)) startPoints))
        (setq endPoints (cons (cdr (assoc 11 entData)) endPoints))
        (setq ss (ssdel (ssname ss 0) ss))  ; ���� ���տ��� ��ü ����
      )

      (setq sortedPoints (vl-sort startPoints 
                                  '(lambda (p1 p2)
                                     (if (< (distance (car p1) (car p2)) tolerance) 
                                         (distance (car p1) (car p2)) 
                                         0.0))))

      (setq polyPoints '())
      (setq currentPoint (car sortedPoints))
      (setq sortedPoints (cdr sortedPoints))

      (while sortedPoints
        (setq nextPoint nil)
        (foreach pt sortedPoints
          (if (<= (distance currentPoint pt) tolerance)
            (progn
              (setq nextPoint pt)
              (setq sortedPoints (vl-remove pt sortedPoints))
              (setq polyPoints (append polyPoints (list currentPoint)))
              (setq currentPoint nextPoint)
            )
          )
        )
        (if (not nextPoint)
          (setq currentPoint (car sortedPoints)
                sortedPoints (cdr sortedPoints)))
      )
      (setq polyPoints (append polyPoints (list currentPoint)))
      
      (command "_.PLINE")
      (foreach pt polyPoints
        (command (car pt) (cadr pt))
      )
      (command "")
      (princ "\nLines have been converted to a polyline.")
    )
    (prompt "\nNo lines selected.")
  )
  (princ)  ; �Լ� ����
)