(defun get-intersection-points (polyline xInterval startX)
  (setq intersectionPoints '())

  ;; Iterate over the vertices of the polyline
  (setq vertList (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) (entget polyline))))
  (setq numVerts (length vertList))

  (setq i 0)
  (while (< i (- numVerts 1))
    (setq pt1 (nth i vertList))
    (setq pt2 (nth (1+ i) vertList))
    (setq x1 (car pt1) y1 (cadr pt1))
    (setq x2 (car pt2) y2 (cadr pt2))

    ;; Check if the segment crosses any xInterval points starting from startX
    (setq j 0)
    (while (<= (+ startX (* j xInterval)) (max x1 x2))
      (setq x (+ startX (* j xInterval)))
      (if (and (<= (min x1 x2) x (max x1 x2)) (/= x1 x2)) ;; Avoid division by zero
        (setq y (+ y1 (* (/ (- y2 y1) (- x2 x1)) (- x x1)))
              intersectionPoints (cons (list x y 0) intersectionPoints)))
      (setq j (1+ j))
      (if (> x (max x1 x2)) (setq j (+ j (fix (/ (- x (max x1 x2)) xInterval)))))) ; 루프 종료 조건 추가

    (setq i (1+ i)))

  (reverse intersectionPoints))

(defun c:CreateLinesBetweenTwoLines (/ p1 line1 line2 spacing length pts1 pts2 pt1 pt2 i newLine)
  ;; 입력 받기
  (setq p1 (getpoint "\n첫 시작 위치를 지정하세요: "))
  (setq line1 (car (entsel "\n첫 번째 라인을 선택하세요: ")))
  (setq line2 (car (entsel "\n두 번째 라인을 선택하세요: ")))
  (setq spacing (getdist "\n간격을 설정하세요: "))
  (setq length (getdist "\n총 길이를 설정하세요: "))

  ;; 시작 위치의 X 좌표 계산
  (setq startX (car p1))

  ;; 첫 번째 라인에서 점 얻기
  (setq pts1 (get-intersection-points line1 spacing startX))
  ;; 두 번째 라인에서 점 얻기
  (setq pts2 (get-intersection-points line2 spacing startX))

  ;; 두 라인 중 작은 점의 개수를 기준으로 설정
  (setq minPts (min (length pts1) (length pts2)))

  ;; 점들을 간격에 맞게 생성하고 선으로 연결
  (setq i 0)
  (while (< i minPts)
    (setq pt1 (nth i pts1))
    (setq pt2 (nth i pts2))

    ;; 점들 연결하는 선 생성
    (setq newLine (entmake
      (list
        (cons 0 "LINE")
        (cons 10 pt1)
        (cons 11 pt2)
        (cons 6 "CENTER")  ;; 선 유형 설정
      )
    ))
    (setq i (1+ i))
  )
  (princ)
)
