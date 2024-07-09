(defun c:GS (/ p1 p2 ss num i ent entData pt1 pt2 len totalLength numGRBCenterLines averageLength lineData tblObj tblRow tblPos rowHeight colWidth txtHeight)
  ;; 사용자가 드래그하여 선택할 수 있도록 두 개의 점을 입력받음
  (setq p1 (getpoint "\n첫 번째 점을 선택하십시오: "))
  (setq p2 (getcorner p1 "\n두 번째 점을 선택하십시오: "))

  ;; 선택 영역에서 GRB_CENTER 레이어에 있는 모든 라인을 선택
  (setq ss (ssget "C" p1 p2 '((8 . "GRB_CENTER") (0 . "LINE"))))
  
  (if ss
    (progn
      ;; 초기화
      (setq num (sslength ss))
      (setq i 0)
      (setq totalLength 0.0)
      (setq numGRBCenterLines 0)
      (setq lineData '())
      (setq rowHeight 400.0) ;; 행 높이 조정
      (setq colWidth 2000.0) ;; 열 너비 조정
      (setq txtHeight 300) ;; 텍스트 높이 설정

      ;; 선택 집합의 각 선을 순회하며 정보를 계산
      (while (< i num)
        (setq ent (ssname ss i))
        (setq entData (entget ent))
        (setq pt1 (cdr (assoc 10 entData)))
        (setq pt2 (cdr (assoc 11 entData)))
        (setq len (distance pt1 pt2))

        ;; GRB_CENTER 레이어에 있는 선들의 정보를 계산
        (setq totalLength (+ totalLength len))
        (setq numGRBCenterLines (1+ numGRBCenterLines))

        ;; 라인 데이터를 리스트에 추가
        (setq lineData (append lineData (list (list (1+ i) len))))

        (setq i (1+ i)))
      
      ;; 평균 길이를 계산
      (if (> numGRBCenterLines 0)
        (setq averageLength (/ totalLength numGRBCenterLines))
        (setq averageLength 0.0))

      ;; 표 위치 지정
      (setq tblPos (getpoint "\n표 위치를 클릭하십시오: "))

      ;; 도면에 표 생성
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
      (setq modelSpace (vla-get-ModelSpace doc))
      (setq tblObj (vla-AddTable modelSpace (vlax-3d-point tblPos) (+ numGRBCenterLines 2) 2 rowHeight colWidth))

      ;; 표 텍스트 높이 설정
      (vla-SetTextHeight tblObj (+ acDataRow acHeaderRow acTitleRow) txtHeight)

      ;; 표 헤더 설정
      (vla-SetText tblObj 0 0 "번호")
      (vla-SetText tblObj 0 1 "길이")

      ;; 표에 데이터 추가
      (setq tblRow 1)
      (foreach line lineData
        (vla-SetText tblObj tblRow 0 (itoa (car line)))
        (vla-SetText tblObj tblRow 1 (rtos (cadr line) 2 2))
        (setq tblRow (1+ tblRow))
      )

      ;; 합계 길이 추가
      (vla-SetText tblObj tblRow 0 "총 길이")
      (vla-SetText tblObj tblRow 1 (rtos totalLength 2 2))
    )
    (princ "\n선택된 영역에 GRB_CENTER 레이어에 선이 없습니다.")
  )

  (princ)
)

(princ "\nLISP 명령어가 로드되었습니다. 'GS' 명령어를 사용하여 GRB_CENTER 레이어의 선들을 분석하십시오.")
(princ)






	    ;; 마지막 행에 "전체 길이" 추가
	    (setq total-length 0.0)
	    (setq col 2)
	    (while (< col (1+ (length (car table_list))))
	      (setq total-length (+ total-length (rtos (sum-list (nth (1- col) table_list)) 2 2)))
	      (setq col (1+ col))
	    )
	    (vla-SetText tblObj (1+ row) 0 "전체 길이")
	    (vla-SetText tblObj (1+ row) col (rtos total-length 2 2))