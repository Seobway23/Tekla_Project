(defun c:GV (/ polylineObj1 polylineObj2 xInterval diameter thickness startPt intersectionPoints1 intersectionPoints2)
  ;; 시작 위치를 입력받음
  (setq startPt (getpoint "\nDP 시작 위치를 선택하십시오: "))

  ;; 위쪽 폴리선을 선택
  (setq polylineObj1 (car (entsel "\n위쪽 폴리선을 선택하십시오: ")))
  
  ;; 아래쪽 폴리선을 선택
  (setq polylineObj2 (car (entsel "\n아래쪽 폴리선을 선택하십시오: ")))

  ;; X 간격 입력
  (setq xInterval (getreal "\n간격을 입력하십시오: "))

  ;; 두 개의 선택된 객체가 모두 폴리선인지 확인
  (if (and polylineObj1 polylineObj2
           (or (= (cdr (assoc 0 (entget polylineObj1))) "LWPOLYLINE")
               (= (cdr (assoc 0 (entget polylineObj1))) "LINE"))
           (or (= (cdr (assoc 0 (entget polylineObj2))) "LWPOLYLINE")
               (= (cdr (assoc 0 (entget polylineObj2))) "LINE")))
    (progn

      ;; 전체 길이 입력
      (setq maxLen (getreal "\n전체 길이를 입력하시오:"))
      (setq interval (/ (rem maxLen xInterval) 2))
      (setq num (/ maxLen xInterval))
      
      ;; 직경과 두께를 입력받음
      (setq diameter (getreal "\n직경을 입력하십시오: "))
      (setq thickness (getreal "\n두께를 입력하십시오: "))

      ;; Function to calculate intersection points for a given polyline starting from startPt
      (defun get-intersection-points (polyline xInterval startPt)
        (setq intersectionPoints '())

        ;; 시작 위치의 X 좌표를 기준으로 간격을 계산
        (setq startX (car startPt))
        
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
          (setq j (fix (/ (- x1 startX) xInterval)))
          (while (<= (+ startX (* j xInterval)) x2)
            (setq x (+ startX (* j xInterval)))
            (if (and (<= x1 x x2) (/= x1 x2)) ;; Avoid division by zero
              (setq y (+ y1 (* (/ (- y2 y1) (- x2 x1)) (- x x1)))
                    intersectionPoints (cons (list x y 0) intersectionPoints)))
            (setq j (1+ j))
            (if (> x (+ startX (* j xInterval))) (setq j (1+ j)))) ; 루프 종료 조건 추가

          (setq i (1+ i)))

        (reverse intersectionPoints))
      
      ;; 위쪽 폴리선과 아래쪽 폴리선의 교차점을 계산
      (setq intersectionPoints1 (get-intersection-points polylineObj1 xInterval startPt))
      (setq intersectionPoints2 (get-intersection-points polylineObj2 xInterval startPt))

      ;; 두 폴리선의 교차점이 동일한 수인지 확인
      (if (= (length intersectionPoints1) (length intersectionPoints2))
        (progn
          ;; 각 교차점에 대해 DP 명령을 실행
          (defun draw-dp (pt1 pt2 diameter thickness)
            (setq height (- (cadr pt1) (- (cadr pt2) 1000))) ;; 높이는 pt1과 pt2의 y차이로 설정
            (setq radius (/ diameter 2))
            
            ;; 선의 양 끝점을 구하여 선을 그리기
            (setq prd (list (+ (car pt1) radius) (cadr pt1) (caddr pt1))) ; 오른쪽 점
            (setq pld (list (- (car pt1) radius) (cadr pt1) (caddr pt1))) ; 왼쪽 점
            (setq prt (list (- (car prd) thickness) (cadr prd) (caddr prd))) ; 두께 반영한 오른쪽 점
            (setq plt (list (+ (car pld) thickness) (cadr pld) (caddr pld))) ; 두께 반영한 왼쪽 점

            ;; prd와 pld를 연결하는 선 그리기
            (entmake (list (cons 0 "LINE") 
                           (cons 8 "GRB강관말뚝") 
                           (cons 62 3) 
                           (cons 10 prd) 
                           (cons 11 pld))) ; 기본색

            ;; 각 점을 Y축으로 -height만큼 이동한 좌표 계산
            (setq prd2 (list (car prd) (- (cadr prd) height) (caddr prd)))
            (setq pld2 (list (car pld) (- (cadr pld) height) (caddr pld)))

            ;; CENTER 선 유형을 사용하여 중심선 그리기
            (entmake (list (cons 0 "LINE")
                           (cons 8 "GRB강관말뚝")        ; 레이어 설정
                           (cons 62 1)          ; 색상 설정 (빨간색)
                           (cons 6 "CENTER")    ; 선 유형 설정
                           (cons 10 pt1) 
                           (cons 11 (list (car pt1) (- (cadr pt1) height) (caddr pt1))))) ; 끝점 설정

            ;; 수직으로 나머지 선들 그리기
            (entmake (list (cons 0 "LINE") 
                           (cons 8 "GRB강관말뚝") 
                           (cons 62 3)          ; 색상 설정 (녹색)
                           (cons 10 prd) 
                           (cons 11 prd2))) ; 끝점 설정

            (entmake (list (cons 0 "LINE") 
                           (cons 8 "GRB강관말뚝") 
                           (cons 62 3)          ; 색상 설정 (녹색)
                           (cons 10 pld) 
                           (cons 11 pld2))) ; 끝점 설정

            (entmake (list (cons 0 "LINE") 
                           (cons 8 "GRB강관말뚝") 
                           (cons 62 1)          ; 색상 설정 (빨간색)
                           (cons 10 plt) 
                           (cons 11 (list (car plt) (- (cadr plt) height) (caddr plt))))) ; 끝점 설정

            (entmake (list (cons 0 "LINE") 
                           (cons 8 "GRB강관말뚝") 
                           (cons 62 1)          ; 색상 설정 (빨간색)
                           (cons 10 prt) 
                           (cons 11 (list (car prt) (- (cadr prt) height) (caddr prt))))) ; 끝점 설정

            ;; prd2와 pld2를 연결
            (entmake (list (cons 0 "LINE") 
                           (cons 8 "GRB강관말뚝") 
                           (cons 62 3)          ; 색상 설정 (녹색)
                           (cons 10 prd2) 
                           (cons 11 pld2))) ; 기본색
          )

          ;; 각 점에 대해 DP 명령 실행
          (foreach pt1 intersectionPoints1
            (setq pt2 (car intersectionPoints2))
            ;; 두 번째 점을 y - 1000으로 오프셋
            (setq pt2 (list (car pt2) (- (cadr pt2) 1000) (caddr pt2)))
            (draw-dp pt1 pt2 diameter thickness)
            ;; Remove the used point from the second list
            (setq intersectionPoints2 (cdr intersectionPoints2)))
          (princ "\n폴리선 사이의 지정된 x 간격에서 DP 요소가 생성되었습니다."))
        (princ "\n두 폴리선의 x-간격 점이 일치하지 않습니다."))
      )
    (princ "\n하나 또는 두 개의 선택된 객체가 폴리선이 아닙니다."))
  (princ))

(princ "\nLISP 명령을 로드하였습니다. 'IntPile' 명령을 사용하십시오.")
(princ)
