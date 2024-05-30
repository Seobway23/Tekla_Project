(defun c:FindCircle ( / p1 p2 ss i entType)
  ;; 첫 번째 점 선택
  (setq p1 (getpoint "\n첫 번째 점을 선택하세요: "))
  ;; 두 번째 점 선택
  (setq p2 (getcorner p1 "\n반대쪽 점을 선택하세요: "))

  ;; 선택된 범위 내의 객체들 선택
  (setq ss (ssget "C" p1 p2))

  ;; 선택된 객체가 있는지 확인
  (if ss
    (progn
      (setq i 0)
      ;; 선택된 객체들을 순회하며 정보 출력
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq entType (cdr (assoc 0 (entget ent))))
        
        ;; 객체 타입 출력
        (princ (strcat "\nEntity " (itoa i) ": " entType))

        ;; 객체가 원(CIRCLE)인지 확인
        (if (equal entType "CIRCLE")
          (progn
            (setq center (cdr (assoc 10 (entget ent))))
            (setq radius (cdr (assoc 40 (entget ent))))
            (princ (strcat "\n이 객체는 원입니다. 중심점: "
                           (rtos (car center) 2 2) ", "
                           (rtos (cadr center) 2 2) ", "
                           (rtos (caddr center) 2 2) 
                           " 반지름: " (rtos radius 2 2))))
        )
        
        (setq i (1+ i)))
    )
    (princ "\n선택된 객체가 없습니다.")
  )

  (princ)
)

