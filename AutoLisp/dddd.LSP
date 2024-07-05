(defun c:GV (/ polylineObj1 polylineObj2 xInterval startPt intersectionPoints1 intersectionPoints2 minLenPolyline maxLenPolyline)
  ;; 시작 위치를 입력받음
  (setq startPt (getpoint "\nDP 시작 위치를 선택하십시오: "))

  ;; 첫 번째 선을 선택
  (setq polylineObj1 (car (entsel "\n첫 번째 선을 선택하십시오: ")))
  
  ;; 두 번째 선을 선택
  (setq polylineObj2 (car (entsel "\n두 번째 선을 선택하십시오: ")))

  ;; X 간격 입력
  (setq xInterval (getreal "\n간격을 입력하십시오: "))

  ;; 두 선의 길이를 X축을 기준으로 계산
  (defun get-length-x (obj)
    (setq startX (car (vlax-curve-getStartPoint obj)))
    (setq endX (car (vlax-curve-getEndPoint obj)))
    (abs (- endX startX)))

  (setq len1 (get-length-x polylineObj1))
  (setq len2 (get-length-x polylineObj2))
  
  ;; 짧은 선과 긴 선을 구분
  (if (< len1 len2)
    (progn
      (setq minLenPolyline polylineObj1)
      (setq maxLenPolyline polylineObj2))
    (progn
      (setq minLenPolyline polylineObj2)
      (setq maxLenPolyline polylineObj1)))

  ;; 짧은 선의 교차점 생성 함수
  (defun get-intersection-points (polyline xInterval startPt)
    (setq intersectionPoints '())

    ;; 시작 위치의 X 좌표를 기준으로 간격을 계산
    (setq startX (car startPt))
    
    ;; 폴리선의 정점들을 순회
    (setq vertList (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) (entget polyline))))
    (setq numVerts (length vertList))

    (setq i 0)
    (while (< i (- numVerts 1))

      (setq pt1 (nth i vertList))
      (setq pt2 (nth (1+ i) vertList))
      (setq x1 (car pt1) y1 (cadr pt1))
      (setq x2 (car pt2) y2 (cadr pt2))

      ;; 시작 위치의 X 좌표로부터 xInterval 포인트들을 계산
      (setq j (fix (/ (- x1 startX) xInterval)))
      (while (<= (+ startX (* j xInterval)) (max x1 x2))
        (setq x (+ startX (* j xInterval)))
        (if (and (<= (min x1 x2) x (max x1 x2)) (/= x1 x2)) ;; Division by zero 방지
          (setq y (+ y1 (* (/ (- y2 y1) (- x2 x1)) (- x x1)))
                intersectionPoints (cons (list x y 0) intersectionPoints)))
        (setq j (1+ j))
        (if (> x (+ startX (* j xInterval))) (setq j (1+ j)))) ;; 루프 종료 조건 추가

      (setq i (1+ i)))

    (reverse intersectionPoints))

  (defun calculate-intersection-points (shortPoints longPolyline)
  (setq intersectionPoints2 '())

  ;; 폴리선의 정점들을 순회하지 않고 shortPoints 리스트를 순회하면서 각 점의 x좌표와 교차하는 긴 선의 y좌표를 계산
  (foreach pt shortPoints
    (setq x (car pt))
    (setq y nil) ;; y 초기화

    ;; 긴 선의 세그먼트와 x좌표의 교차점 계산
    (setq verts (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) (entget longPolyline))))
    (setq numVerts (length verts))
    (setq i 0)
    (while (and (< i (- numVerts 1)) (not y))
      (setq pt1 (nth i verts))
      (setq pt2 (nth (1+ i) verts))
      (setq x1 (car pt1) y1 (cadr pt1))
      (setq x2 (car pt2) y2 (cadr pt2))

      ;; x 좌표가 세그먼트 내에 있는지 확인
      (if (and (/= x1 x2) (<= (min x1 x2) x (max x1 x2)))
        (setq y (+ y1 (* (/ (- y2 y1) (- x2 x1)) (- x x1)))))
      
      (setq i (1+ i)))

    ;; y 값을 intersectionPoints2에 추가
    (if y
      (setq intersectionPoints2 (cons (list x y 0) intersectionPoints2))))

  (reverse intersectionPoints2))


  ;; 짧은 선의 교차점을 계산
  (setq intersectionPoints1 (get-intersection-points minLenPolyline xInterval startPt))
  ;; 짧은 선의 교차점이 올바르게 계산되었는지 확인
  (princ "\nintersectionPoints1: ")
  (princ (vl-princ-to-string intersectionPoints1))
  
  ;; 긴 선의 교차점을 계산
  (setq intersectionPoints2 (calculate-intersection-points intersectionPoints1 maxLenPolyline))
  ;; 긴 선의 교차점이 올바르게 계산되었는지 확인
  (princ "\nintersectionPoints2: ")
  (princ (vl-princ-to-string intersectionPoints2))

  ;; 포인트 생성 함수
  (defun create-point (pt)
    (entmake (list (cons 0 "POINT")
                   (cons 10 pt)
                   (cons 8 "GRB강관말뚝"))))

  ;; 짧은 선의 포인트를 생성
  (foreach pt intersectionPoints1
    (create-point pt))

  ;; 긴 선의 포인트를 생성
  (foreach pt intersectionPoints2
    (create-point pt))

  (princ "\n폴리선 사이의 지정된 x 간격에서 포인트가 생성되었습니다.")
  (princ))

(princ "\nLISP 명령을 로드하였습니다. 'IntPile' 명령을 사용하십시오.")
(princ)
