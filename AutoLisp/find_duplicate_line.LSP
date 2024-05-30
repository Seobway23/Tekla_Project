(defun line-length (startPt endPt)
  (sqrt (+ (expt (- (car endPt) (car startPt)) 2)
           (expt (- (cadr endPt) (cadr startPt)) 2))))

(defun lines-overlap (line1 line2)
  (or (and (equal (car line1) (car line2)) (equal (cadr line1) (cadr line2)))
      (and (equal (car line1) (cadr line2)) (equal (cadr line1) (car line2)))))

(defun c:FindDuplicateLine ( / p1 p2 ss i entType startPt endPt pointsList)
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
      (setq pointsList '())  ;; 시작과 끝점을 저장할 리스트 초기화
      ;; 선택된 객체들을 순회하며 정보 출력
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq entType (cdr (assoc 0 (entget ent))))

        ;; 객체의 시작과 끝점 추출
        (cond
          ;; LINE 객체 처리
          ((equal entType "LINE")
           (setq startPt (cdr (assoc 10 (entget ent))))
           (setq endPt (cdr (assoc 11 (entget ent))))
           (setq pointsList (cons (list startPt endPt ent) pointsList)))
        )
        (setq i (1+ i)))

      ;; 겹치는 라인 제거
      (setq i 0)
      (while (< i (length pointsList))
        (setq j (1+ i))
        (setq line1 (nth i pointsList))
        (setq length1 (line-length (car line1) (cadr line1)))
        (while (< j (length pointsList))
          (setq line2 (nth j pointsList))
          (setq length2 (line-length (car line2) (cadr line2)))
          (if (lines-overlap line1 line2)
            (if (< length1 length2)
              (entdel (caddr line1))
              (entdel (caddr line2))))
          (setq j (1+ j)))
        (setq i (1+ i)))

      ;; 라인들 출력
      (princ "\n라인 리스트:")
      (mapcar
        '(lambda (pts)
           (princ (strcat "\n시작점: "
                          (rtos (car (car pts)) 2 2) ", "
                          (rtos (cadr (car pts)) 2 2) ", "
                          (rtos (caddr (car pts)) 2 2)
                          " - 끝점: "
                          (rtos (car (cadr pts)) 2 2) ", "
                          (rtos (cadr (cadr pts)) 2 2) ", "
                          (rtos (caddr (cadr pts)) 2 2))))
        pointsList)
    )
    (princ "\n선택된 객체가 없습니다.")
  )

  (princ)
)
