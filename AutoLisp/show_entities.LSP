(defun c:ShowEntities ( / ss entData uniqueId dxf0 dxf40 dxf100 sx sy ex ey cx cy radius startAngle endAngle midAngle pointList ent file)
  (setq ss (ssget))  ; 사용자가 선택한 객체들을 선택 집합으로 반환
  (if ss
    (progn
      (setq file (open "C:\\Users\\admin\\Desktop\\Tekla_Project\\AutoLisp\\data.csv" "w"))  ; CSV 파일 경로 설정 및 파일 오픈
      (write-line "Entity ID,Type,DXF40,Model,Points" file)  ; CSV 헤더 작성
      (setq uniqueId 0)  ; 고유 번호 초기화
      (repeat (sslength ss)  ; 선택 집합의 크기만큼 반복
        (setq ent (ssname ss uniqueId))  ; uniqueId 인덱스의 객체의 핸들을 얻음
        (setq entData (entget ent))  ; 엔티티 데이터를 가져옴
        (setq dxf0 (cdr (assoc 0 entData)))
        (setq dxf40 (cdr (assoc 40 entData)))
        (setq dxf40 (if dxf40 (rtos dxf40 2 2) "0.0"))  ; DXF 40이 없으면 0.0으로 설정
        (setq dxf100 (cdr (assoc 100 entData)))
        (setq pointList '())  ; 포인트 데이터 리스트 초기화

        (foreach pt (vl-remove-if-not '(lambda (x) (= (car x) 10)) entData)
          (setq x (nth 0 (cdr pt)))
          (setq y (nth 1 (cdr pt)))
          (setq pointList (cons (list (rtos x 2 2) (rtos y 2 2)) pointList))
        )
        
        (if (assoc 11 entData)
          (foreach pt (vl-remove-if-not '(lambda (x) (= (car x) 11)) entData)
            (setq x (nth 0 (cdr pt)))
            (setq y (nth 1 (cdr pt)))
            (setq pointList (cons (list (rtos x 2 2) (rtos y 2 2)) pointList))
          )
        )

        (cond
          ((= dxf0 "ARC")  ; 엔티티 타입이 ARC인 경우
            (setq cx (nth 0 (cdr (assoc 10 entData))))  ; 중심점 X 좌표
            (setq cy (nth 1 (cdr (assoc 10 entData))))  ; 중심점 Y 좌표
            (setq radius (cdr (assoc 40 entData)))  ; 반지름
            (setq startAngle (* (cdr (assoc 50 entData)) (/ pi 180.0)))  ; 시작각 (라디안으로 변환)
            (setq endAngle (* (cdr (assoc 51 entData)) (/ pi 180.0)))  ; 끝각 (라디안으로 변환)
            (setq midAngle (/ (+ startAngle endAngle) 2.0))  ; 중간각
            (setq sx (+ cx (* radius (cos startAngle))))  ; 시작점 X 좌표
            (setq sy (+ cy (* radius (sin startAngle))))  ; 시작점 Y 좌표
            (setq ex (+ cx (* radius (cos endAngle))))  ; 끝점 X 좌표
            (setq ey (+ cy (* radius (sin endAngle))))  ; 끝점 Y 좌표
            (setq mx (+ cx (* radius (cos midAngle))))  ; 중간점 X 좌표
            (setq my (+ cy (* radius (sin midAngle))))  ; 중간점 Y 좌표
            (setq pointList (list
                              (list (rtos sx 2 2) (rtos sy 2 2))
                              (list (rtos mx 2 2) (rtos my 2 2))
                              (list (rtos ex 2 2) (rtos ey 2 2))
                            ))  ; 포인트 리스트에 시작점, 중간점, 끝점 추가
          )
        )

        ; CSV 파일에 데이터 쓰기
        (write-line 
          (strcat 
            (itoa uniqueId) "," 
            dxf0 "," 
            dxf40 "," 
            dxf100 ",\"" 
            (apply 'strcat (mapcar '(lambda (pt) (strcat "(" (car pt) ", " (cadr pt) ") ")) pointList)) 
            "\""
          ) 
          file
        )

        (setq uniqueId (+ uniqueId 1))  ; 고유 번호 증가
      )
      (close file)  ; 파일 닫기
      (princ "\nData has been exported to CSV successfully.")
    )
    (prompt "\nNo objects selected.")  ; 객체 선택되지 않았을 경우 메시지
  )
  (princ)  ; 함수 종료
)
