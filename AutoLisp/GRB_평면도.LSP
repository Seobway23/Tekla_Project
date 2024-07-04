(defun c:GP (/ polylineObj xInterval diameter thickness startPt intersectionPoints maxLen Interval num)
  ;; 시작 위치를 입력받음
  (setq startPt (getpoint "\nDP 시작 위치를 선택하십시오: "))

  ;; 폴리선을 선택
  (setq polylineObj (vlax-ename->vla-object (car (entsel "\n폴리선을 선택하십시오: "))))

  ;; X 간격 입력
  (setq xInterval (getreal "\n간격을 입력하십시오: "))

  ;; 보강 길이 입력
  (setq maxLen (getreal "\n전체 길이를 입력하십시오: "))
  (setq Interval (/ (rem maxLen xInterval) 2))
  (setq num (/ maxLen xInterval))

  ;; 직경과 두께를 입력받음
  (setq diameter (getreal "\n직경을 입력하십시오: "))
  (setq thickness (getreal "\n두께를 입력하십시오: "))

  ;; 폴리라인의 길이 계산
  (setq polyLength (vlax-curve-getDistAtPoint polylineObj (vlax-curve-getEndPoint polylineObj)))

  ;; 현재 위치 초기화 (Interval만큼 이동)
  (setq currentDist (+ (vlax-curve-getDistAtPoint polylineObj startPt) Interval))

  (setq count 0)
  (while (and (< currentDist polyLength) (< count num))
    ;; 현재 위치에서의 좌표 계산
    (setq currentPt (vlax-curve-getPointAtDist polylineObj currentDist))

    ;; 각 교차점에 대해 원을 그림
    (setq radius (/ diameter 2))
    ;; 외곽 원 그리기
    (entmake
      (list
        (cons 0 "CIRCLE")
        (cons 8 "GRB강관말뚝") 
        (cons 10 currentPt)
        (cons 40 radius)
        (cons 62 1)  ; 빨간색
      )
    )
    ;; 두께를 반영한 내부 원 그리기
    (entmake
      (list
        (cons 0 "CIRCLE")
        (cons 8 "GRB강관말뚝") 
        (cons 6 "CENTER")
        (cons 10 currentPt)
        (cons 40 (- radius thickness))
        (cons 62 1)  ; 빨간색
      )
    )

    ;; 간격을 따라 이동
    (setq currentDist (+ currentDist xInterval))
    (setq count (1+ count))
  )

  (princ "\n지정된 간격에서 원이 생성되었습니다.")
)

(princ "\nLISP 명령을 로드하였습니다. 'MakePile' 명령을 사용하십시오.")
(princ)
