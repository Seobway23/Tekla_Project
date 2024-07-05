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

  ;; 초록색 선들을 하나의 폴리라인으로 연결
  (entmake (list (cons 0 "POLYLINE")
                 (cons 8 "GRB강관말뚝") 
                 (cons 62 3)          ; 색상 설정 (녹색)
                 (cons 66 1)))        ; 꼭지점 모드 설정

  ;; 폴리라인에 꼭지점 추가
  (foreach pt (list prd prd2 pld2 pld)
    (entmake (list (cons 0 "VERTEX")
                   (cons 8 "GRB강관말뚝")
                   (cons 10 pt))))

  ;; 폴리라인 종료
  (entmake (list (cons 0 "SEQEND")
                 (cons 8 "GRB강관말뚝")))
)

(princ "\nLISP 명령을 로드하였습니다. 'draw-dp' 명령을 사용하십시오.")
(princ)
