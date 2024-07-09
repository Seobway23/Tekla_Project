(defun c:GV (/ polylineObj1 polylineObj2 xInterval diameter thickness startPt intersectionPoints1 intersectionPoints2 maxLen interval num endPt EP1 gap newPts)

  (if (not (tblsearch "LTYPE" "CENTER"))
    (command "._-linetype" "Load" "CENTER" "acad.lin")
  )

  ;; 시작 위치를 입력받음
  (setq startPt (getpoint "\n 말뚝상단 시점 시작 위치를 선택하십시오: "))

  ;; 위쪽 객체를 선택
  (while (not polylineObj1)
    (setq polylineObj1 (car (entsel "\n 말뚝 상단 객체를 선택하십시오: ")))
    (if (not polylineObj1)
      (princ "\n올바른 객체를 선택하십시오.")))

  ;; 아래쪽 객체를 선택
  (while (not polylineObj2)
    (setq polylineObj2 (car (entsel "\n말뚝 하단 객체를 선택하십시오: ")))
    (if (not polylineObj2)
      (princ "\n올바른 객체를 선택하십시오.")))

  ;; X 간격 입력
  (setq xInterval (getreal "\n간격을 입력하십시오: "))

  ;; 전체 길이 입력
  (setq maxLen (getreal "\n전체 길이를 입력하십시오:"))  
  (setq interval (/ xInterval 2))
  (setq num (fix (/ maxLen xInterval)))
  (setq num (+ num 1))

  ;; 직경과 두께를 입력받음
  (setq diameter (getreal "\n직경을 입력하십시오: "))
  (setq thickness (getreal "\n두께를 입력하십시오: "))


  ;; 교차점 찾는 함수 
  (defun get-intersection-points-from-start (entity xInterval startPt num)
  (setq intersectionPoints '())

  ;; 시작 위치의 X 좌표를 기준으로 간격을 계산
  (setq startX (car startPt))

  ;; Entity type 확인
  (setq entityType (cdr (assoc 0 (entget entity))))

  (cond
    ((equal entityType "LWPOLYLINE")
     ;; 폴리라인의 경우
     (setq vertList (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) (entget entity))))
     (setq numVerts (length vertList))
     (setq i 0 count 0)
     (while (and (< i (- numVerts 1)) (< count num))
       (setq pt1 (nth i vertList))
       (setq pt2 (nth (1+ i) vertList))
       (setq x1 (car pt1) y1 (cadr pt1))
       (setq x2 (car pt2) y2 (cadr pt2))
       (setq j (fix (/ (- startX x1) xInterval)))
       (while (and (<= (+ x1 (* j xInterval)) x2) (< count num))
         (setq x (+ x1 (* j xInterval)))
         (if (and (<= x1 x x2) (/= x1 x2))
           (setq y (+ y1 (* (/ (- y2 y1) (- x2 x1)) (- x x1)))
                 intersectionPoints (cons (list x y 0) intersectionPoints)
                 count (1+ count)))
         (setq j (1+ j)))
       (setq i (1+ i))))
    ((equal entityType "LINE")
     ;; 라인의 경우
     (setq entData (entget entity))
     (setq pt1 (cdr (assoc 10 entData)))
     (setq pt2 (cdr (assoc 11 entData)))
     (setq vertList (list pt1 pt2)) ; 라인을 폴리라인처럼 점 리스트로 변환
     (setq numVerts (length vertList))
     (setq i 0 count 0)
     (while (and (< i (- numVerts 1)) (< count num))
       (setq pt1 (nth i vertList))
       (setq pt2 (nth (1+ i) vertList))
       (setq x1 (car pt1) y1 (cadr pt1))
       (setq x2 (car pt2) y2 (cadr pt2))
       (setq j (fix (/ (- startX x1) xInterval)))
       (while (and (<= (+ x1 (* j xInterval)) x2) (< count num))
         (setq x (+ x1 (* j xInterval)))
         (if (and (<= x1 x x2) (/= x1 x2))
           (setq y (+ y1 (* (/ (- y2 y1) (- x2 x1)) (- x x1)))
                 intersectionPoints (cons (list x y 0) intersectionPoints)
                 count (1+ count)))
         (setq j (1+ j)))
       (setq i (1+ i)))))
  (reverse intersectionPoints))

  
  ;; 위쪽 객체와 아래쪽 객체의 교차점을 계산
  (setq intersectionPoints1 (get-intersection-points polylineObj1 xInterval (list (+ (car startPt) interval) (cadr startPt) (caddr startPt)) num))
  (setq intersectionPoints2 (get-intersection-points polylineObj2 xInterval (list (+ (car startPt) interval) (cadr startPt) (caddr startPt)) num))

  ;; 두 객체의 교차점이 동일한 수인지 확인
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

        ;; 각 점을 Y축으로 -height만큼 이동한 좌표 계산
        (setq prd2 (list (car prd) (- (cadr prd) height) (caddr prd)))
        (setq pld2 (list (car pld) (- (cadr pld) height) (caddr pld)))

        ;; CENTER 선 유형을 사용하여 중심선 그리기
        (entmake (list (cons 0 "LINE")
                       (cons 8 "GRB_CENTER")        ; 레이어 설정
                       (cons 62 1)          ; 색상 설정 (빨간색)
                       (cons 6 "CENTER")    ; 선 유형 설정
                       (cons 10 pt1) 
                       (cons 11 (list (car pt1) (- (cadr pt1) height) (caddr pt1))))) ; 끝점 설정

	    ;; prd와 pld를 연결하는 선 그리기
        (entmake (list (cons 0 "LINE") 
                       (cons 8 "GRB강관말뚝") 
                       (cons 62 3) 
                       (cons 10 prd) 
                       (cons 11 pld))) ; 기본색

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

      (setq count 0)
      (setq EP1 nil)
      (setq previousPt1 nil)
      (setq previousPt2 nil)
      (setq endPt (- (+ (car startPt) maxLen) interval))
      (setq ENDPP (- endPt (* xInterval 2)))

      (while (and (< count num) intersectionPoints1 intersectionPoints2)
	(setq pt1 (car intersectionPoints1))
	(setq pt2 (car intersectionPoints2))
	(setq currentX (car pt1))
	(if (> currentX (-(- endPt (* interval 2)) (/ xInterval 2)))

	  (progn
	    (setq EP1 previousPt1) ;; 이전 pt1 값을 EP1에 저장
	    (setq EP2 previousPt2)

	    (setq intersectionPoints1 nil)
	    (setq intersectionPoints2 nil))

	  (progn
	    (draw-dp pt1 pt2 diameter thickness)
	    (setq previousPt1 pt1)
	    (setq previousPt2 pt2)

	    (setq intersectionPoints1 (cdr intersectionPoints1))
	    (setq intersectionPoints2 (cdr intersectionPoints2))
	    (setq count (1+ count)))))

	(if
	  EP1
	  (progn
	    (setq endInterval (/ (- endPt (car EP1)) 2))
	    
	    (setq endThreePoints1 (get-end-points polylineObj1 endInterval EP1 3))
	    (setq endThreePoints2 (get-end-points polylineObj2 endInterval EP2 3))

	    (setq i 0)
	    (setq endThreePoints1 (cdr endThreePoints1))
	    (setq endThreePoints2 (cdr endThreePoints2))
	   
	    (while (< i 2)
	      (setq et1 (car endThreePoints1))
	      (setq et2 (car endThreePoints2))
	      (progn
		(draw-dp et1 et2 diameter thickness)
		(setq endThreePoints1 (cdr endThreePoints1))
		(setq endThreePoints2 (cdr endThreePoints2))
		(setq i (1+ i))
	      )
	   )
	   )  
	  )

      (princ "\n폴리선 사이의 지정된 x 간격에서 DP 요소가 생성되었습니다.")

      ;; Select and pedit lines on "GRB강관말뚝" layer
      (setq ss (ssget "_X" (list (cons 8 "GRB강관말뚝"))))
      
      ; Select lines on "GRB강관말뚝" layer
      (command "_.pedit" "_multiple" ss "" "_yes" "_join" "" "" "")
	
    )
  (princ "\n하나 또는 두 개의 선택된 객체가 폴리선 또는 라인이 아닙니다."))
(princ))

(princ "\nLISP 명령을 로드하였습니다. 'GV' 명령을 사용하십시오.")
(princ)



(defun get-end-points (entity xInterval startPt num)
  (setq intersectionPoints '())

  ;; 시작 위치의 X 좌표를 기준으로 간격을 계산
  (setq startX (car startPt))

  ;; 현재 위치 초기화
  (setq currentX startX)
  (setq i 0)

  ;; 교차점 찾기
  (while (< i num)
    ;; 현재 위치의 매개변수 값을 구함
    (setq currentPt (vlax-curve-getClosestPointTo entity (list currentX (cadr startPt) (caddr startPt))))
    ;; 교차점 리스트에 추가
    (setq intersectionPoints (append intersectionPoints (list currentPt)))
    ;; 다음 위치로 이동
    (setq currentX (+ currentX xInterval))
    ;; 인덱스 증가
    (setq i (1+ i)))
  
 intersectionPoints)

