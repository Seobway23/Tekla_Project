(defun c:PrintEntitiesInWindow ( / p1 p2 ss i entType startPt endPt pointsList overlapList)
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
      (setq overlapList '()) ;; 겹치는 라인의 정보를 저장할 리스트 초기화
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
           (setq pointsList (cons (list startPt endPt (line-length startPt endPt) ent) pointsList)))
        )
        (setq i (1+ i)))

      ;; 겹치는 라인 찾기
      (setq i 0)
      (while (< i (length pointsList))
        (setq j (1+ i))
        (setq line1 (nth i pointsList))
        (setq length1 (nth 2 line1))
        (while (< j (length pointsList))
          (setq line2 (nth j pointsList))
          (setq length2 (nth 2 line2))
          (if (lines-overlap line1 line2)
            (progn
              (setq overlapList (cons line1 overlapList))
              (setq overlapList (cons line2 overlapList))))
          (setq j (1+ j)))
        (setq i (1+ i)))

      ;; 겹친 라인 정보 출력
      (princ "\n겹치는 라인 리스트:")
      (foreach pts overlapList
        (princ (strcat "\n시작점: "
                       (rtos (car (car pts)) 2 2) ", "
                       (rtos (cadr (car pts)) 2 2)
                       " - 끝점: "
                       (rtos (car (cadr pts)) 2 2) ", "
                       (rtos (cadr (cadr pts)) 2 2)
                       " - 길이: "
                       (rtos (nth 2 pts) 2 2))))
    )
    (princ "\n선택된 객체가 없습니다.")
  )

  (princ)
)

(defun line-length (startPt endPt)
  (sqrt (+ (expt (- (car endPt) (car startPt)) 2)
           (expt (- (cadr endPt) (cadr startPt)) 2))))

(defun lines-overlap (line1 line2)
  (or (and (equal (car line1) (car line2)) (equal (cadr line1) (cadr line2)))
      (and (equal (car line1) (cadr line2)) (equal (cadr line1) (car line2)))))
