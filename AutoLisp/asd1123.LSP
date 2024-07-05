(defun c:GS (/ p1 p2 ss num i ent entData pt1 pt2 len totalLength numGRBCenterLines averageLength lineData tblObj tblRow tblPos rowHeight colWidth txtHeight divideLines divPoints dividedLineData grbLines grbLineEnt intersectionPoints sortedIntersections)

  ;; 사용자가 드래그하여 선택할 수 있도록 두 개의 점을 입력받음
  (setq p1 (getpoint "\n첫 번째 점을 선택하십시오: "))
  (setq p2 (getcorner p1 "\n두 번째 점을 선택하십시오: "))

  ;; 선택된 영역에서 GRB_CENTER 레이어의 모든 라인을 선택
  (setq ss (ssget "C" p1 p2 '((8 . "GRB_CENTER") (0 . "LINE"))))

  ;; 선택된 GRB_CENTER 레이어의 라인들을 리스트로 만듬
  (if ss
    (progn
      (setq num (sslength ss))
      (setq i 0)
      (setq grbLines '())
      (while (< i num)
        (setq grbLineEnt (ssname ss i))
        (setq entData (entget grbLineEnt))
        (setq pt1 (cdr (assoc 10 entData)))
        (setq pt2 (cdr (assoc 11 entData)))
        (setq grbLines (append grbLines (list (list grbLineEnt pt1 pt2))))
        (setq i (1+ i))))
    (princ "\nGRB_CENTER 레이어에 라인이 없습니다.")
  )

  ;; 나눌 라인들 선택 (클릭하여 선택)
  (prompt "\n나눌 라인들을 선택하십시오: ")
  (setq divideLines (ssget))

  (if (and grbLines divideLines)
    (progn
      ;; 초기화
      (setq numDivLines (sslength divideLines))
      (setq rowHeight 400.0) ;; 행 높이 조정
      (setq colWidth 2000.0) ;; 열 너비 조정
      (setq txtHeight 300) ;; 텍스트 높이 설정

      ;; GRB_CENTER 라인들의 교차 지점 리스트
      (setq dividedLineData '())

      ;; 센터 라인들 반복
      (foreach grbLine grbLines
        (setq grbLineEnt (car grbLine))
        (setq startPt (cadr grbLine))
        (setq endPt (caddr grbLine))
        (setq intersectionPoints (list startPt endPt))

        ;; 나누기 위한 라인들 반복
        (setq j 0)
        (while (< j numDivLines)
          (setq ent (ssname divideLines j))
          (setq entData (entget ent))
          (setq divPt1 (cdr (assoc 10 entData)))
          (setq divPt2 (cdr (assoc 11 entData)))

          ;; 교차 지점 계산 (단순히 x값으로 비교)
          (if (and (>= (car divPt1) (car startPt)) (<= (car divPt1) (car endPt)))
            (setq intersectionPoints (append intersectionPoints (list divPt1))))
          (if (and (>= (car divPt2) (car startPt)) (<= (car divPt2) (car endPt)))
            (setq intersectionPoints (append intersectionPoints (list divPt2))))

          (setq j (1+ j)))

        ;; 교차 지점들을 정렬
        (setq sortedIntersections (vl-sort intersectionPoints '(lambda (a b) (< (car a) (car b)))))

	(princ "\n 소트된 노드들: ")
	(princ sortedIntersections )
	(princ "\n")

	(princ "\n 노드 번호: ")
	(princ j)
	
        
        ;; 길이 계산
        (setq prevPt (car sortedIntersections))
        (setq partLengths '())
        (foreach pt sortedIntersections
          (if (/= pt prevPt)
            (progn
              (setq partLength (distance prevPt pt))
              (setq partLengths (append partLengths (list partLength)))
              (setq prevPt pt))))

        ;; 라인 데이터를 리스트에 추가
        (setq dividedLineData (append dividedLineData (list partLengths))))


      ;; 표 리버스
      (reverse dividedLineData)

      
      ;; 표 위치 지정
      (setq tblPos (getpoint "\n표 위치를 클릭하십시오: "))

      ;; 3D 좌표로 변환
      (setq tblPos (vlax-3d-point tblPos))

      ;; 도면에 표 생성
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
      (setq modelSpace (vla-get-ModelSpace doc))
      (setq tblObj (vla-AddTable modelSpace tblPos (+ (length dividedLineData) 2) (+ numDivLines 3) rowHeight colWidth))

      ;; 표 텍스트 높이 설정
      (vla-SetTextHeight tblObj acDataRow txtHeight)
      (vla-SetTextHeight tblObj acHeaderRow txtHeight)
      (vla-SetTextHeight tblObj acTitleRow txtHeight)

      ;; 표 헤더 설정
      (vla-SetText tblObj 0 0 "번호")
      (vla-SetText tblObj 0 1 "원래 길이")
      (setq j 2)
      (while (<= j (+ numDivLines 1))
        (vla-SetText tblObj 0 j (strcat "길이 " (itoa (- j 1))))
        (setq j (1+ j)))
      (vla-SetText tblObj 0 j "합계")

      ;; 표에 데이터 추가
      (setq tblRow 1)
      (setq rowNum 1)
      (foreach line dividedLineData
        (vla-SetText tblObj tblRow 0 (itoa rowNum))
        (setq originalLength (apply '+ line))
        (vla-SetText tblObj tblRow 1 (rtos originalLength 2 2))
        (setq j 2)
        (foreach partLength line
          (vla-SetText tblObj tblRow j (rtos partLength 2 2))
          (setq j (1+ j)))
        (vla-SetText tblObj tblRow j (rtos (apply '+ line) 2 2))
        (setq tblRow (1+ tblRow))
        (setq rowNum (1+ rowNum)))
    )
    (princ "\nGRB_CENTER 레이어의 선이 없거나 나눌 라인이 선택되지 않았습니다.")
  )

  (princ)
)

(princ "\nLISP 명령어가 로드되었습니다. 'GS' 명령어를 사용하여 GRB_CENTER 레이어의 선들을 분석하십시오.")
(princ)
