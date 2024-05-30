(defun c:ClearLine ( / ss i j line1 line2 start1 end1 start2 end2 tolerance overlap toDelete)
  ;; 두 점 사이의 거리를 계산하는 함수
  (defun calc-distance (pt1 pt2)
    (sqrt (+ (expt (- (car pt2) (car pt1)) 2)
             (expt (- (cadr pt2) (cadr pt1)) 2)
             (if (and (caddr pt1) (caddr pt2))
               (expt (- (caddr pt2) (caddr pt1)) 2)
               0))))

  ;; 점 비교 함수
  (defun points-equal (pt1 pt2 tol)
    (and (< (abs (- (car pt1) (car pt2))) tol)
         (< (abs (- (cadr pt1) (cadr pt2))) tol)
         (if (and (caddr pt1) (caddr pt2))
           (< (abs (- (caddr pt1) (caddr pt2))) tol)
           t)))

  ;; 두 라인의 범위가 겹치는지 확인
  (defun ranges-overlap (min1 max1 min2 max2)
    (and (<= min2 max1) (<= min1 max2)))

  ;; 두 라인이 겹치는지 확인
  (defun lines-overlap (start1 end1 start2 end2 tol)
    (setq minX1 (min (car start1) (car end1))
          maxX1 (max (car start1) (car end1))
          minY1 (min (cadr start1) (cadr end1))
          maxY1 (max (cadr start1) (cadr end1))
          minX2 (min (car start2) (car end2))
          maxX2 (max (car start2) (car end2))
          minY2 (min (cadr start2) (cadr end2))
          maxY2 (max (cadr start2) (cadr end2)))
    (and (ranges-overlap minX1 maxX1 minX2 maxX2)
         (ranges-overlap minY1 maxY1 minY2 maxY2)))

  ;; 도면 내의 모든 라인을 선택
  (setq tolerance 0.01) ;; 허용 오차 설정

  (defun delete-overlapping-lines (ss)
    (setq toDelete '())
    (setq i 0)
    (while (< i (sslength ss))
      (setq line1 (ssname ss i))
      (setq start1 (vlax-curve-getStartPoint line1))
      (setq end1 (vlax-curve-getEndPoint line1))
      (setq j (1+ i))
      (while (< j (sslength ss))
        (setq line2 (ssname ss j))
        (setq start2 (vlax-curve-getStartPoint line2))
        (setq end2 (vlax-curve-getEndPoint line2))
        ;; 라인이 겹치는 경우
        (if (lines-overlap start1 end1 start2 end2 tolerance)
          (progn
            (setq length1 (calc-distance start1 end1))
            (setq length2 (calc-distance start2 end2))
            ;; 디버깅 출력: 겹치는 라인 정보
            (princ "\n겹치는 라인 발견:")
            (princ (strcat "\nLine 1: (" (rtos (car start1) 2 2) ", " (rtos (cadr start1) 2 2) ") - ("
                                  (rtos (car end1) 2 2) ", " (rtos (cadr end1) 2 2) ") 길이: " (rtos length1 2 2)))
            (princ (strcat "\nLine 2: (" (rtos (car start2) 2 2) ", " (rtos (cadr start2) 2 2) ") - ("
                                  (rtos (car end2) 2 2) ", " (rtos (cadr end2) 2 2) ") 길이: " (rtos length2 2 2)))
            ;; 작은 라인을 삭제할 목록에 추가
            (if (< length1 length2)
              (progn
                (princ (strcat "\n작은 라인 삭제 예정: (" (rtos (car start1) 2 2) ", " (rtos (cadr start1) 2 2) ") - ("
                                  (rtos (car end1) 2 2) ", " (rtos (cadr end1) 2 2) ")"))
                (setq toDelete (cons line1 toDelete)))
              (progn
                (princ (strcat "\n작은 라인 삭제 예정: (" (rtos (car start2) 2 2) ", " (rtos (cadr start2) 2 2) ") - ("
                                  (rtos (car end2) 2 2) ", " (rtos (cadr end2) 2 2) ")"))
                (setq toDelete (cons line2 toDelete)))))
          (setq j (1+ j))))
      (setq i (1+ i)))
    ;; 작은 라인 삭제
    (foreach line toDelete
      (entdel line)
      (princ (strcat "\n삭제된 라인: " (rtos (car (vlax-curve-getStartPoint line)) 2 2) ", " (rtos (cadr (vlax-curve-getStartPoint line)) 2 2) " - "
                         (rtos (car (vlax-curve-getEndPoint line)) 2 2) ", " (rtos (cadr (vlax-curve-getEndPoint line)) 2 2)))))

  ;; 전체 선택 집합을 한 번에 검사하여 모든 겹치는 라인 삭제
  (if (setq ss (ssget "X" '((0 . "LINE"))))
    (progn
      (delete-overlapping-lines ss)
      (princ "\n도면 정리가 완료되었습니다."))
    (princ "\n선택된 라인이 없습니다.")
  )

  (princ)
)
