using System;
using System.Collections.Generic;
using System.Windows.Forms;
using Tekla.Structures.Model;
using TSG = Tekla.Structures.Geometry3d;

namespace nuget1
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            Model myModel = new Model();

            if (myModel.GetConnectionStatus())
            {
                // 빔과 기둥의 프로필 및 재질 정의
                string materialString = "S245";
                string profileString = "600X800";

                // 정사각형 빔의 간격 및 높이
                double spacing = 6000.0;
                double height = 4000.0;

                // 좌표 리스트 생성
                List<TSG.Point> startPoints = new List<TSG.Point>
        {
            new TSG.Point(0, 0, 0),
            new TSG.Point(spacing, 0, 0),
            new TSG.Point(spacing, spacing, 0),
            new TSG.Point(0, spacing, 0)
        };

                List<TSG.Point> endPoints = new List<TSG.Point>
        {
            new TSG.Point(0, spacing, 0),
            new TSG.Point(spacing, spacing, 0),
            new TSG.Point(spacing, 0, 0),
            new TSG.Point(0, 0, 0)
        };

                List<TSG.Point> columnStartPoints = new List<TSG.Point>
        {
            new TSG.Point(0, 0, 0),
            new TSG.Point(spacing, 0, 0),
            new TSG.Point(spacing, spacing, 0),
            new TSG.Point(0, spacing, 0)
        };

                List<TSG.Point> columnEndPoints = new List<TSG.Point>
        {
            new TSG.Point(0, 0, height),
            new TSG.Point(spacing, 0, height),
            new TSG.Point(spacing, spacing, height),
            new TSG.Point(0, spacing, height)
        };

                // 정사각형 빔 생성
                for (int i = 0; i < startPoints.Count; i++)
                {
                    Beam beam = new Beam
                    {
                        StartPoint = startPoints[i],
                        EndPoint = endPoints[i],
                        Profile = new Profile { ProfileString = profileString },
                        Material = new Material { MaterialString = materialString }
                    };
                    beam.Insert();
                }

                // 위쪽 정사각형 빔 생성
                for (int i = 0; i < startPoints.Count; i++)
                {
                    Beam topBeam = new Beam
                    {
                        StartPoint = new TSG.Point(startPoints[i].X, startPoints[i].Y, height),
                        EndPoint = new TSG.Point(endPoints[i].X, endPoints[i].Y, height),
                        Profile = new Profile { ProfileString = profileString },
                        Material = new Material { MaterialString = materialString }
                    };
                    topBeam.Insert();
                }

                // 기둥 생성
                for (int i = 0; i < columnStartPoints.Count; i++)
                {
                    Beam column = new Beam
                    {
                        StartPoint = columnStartPoints[i],
                        EndPoint = columnEndPoints[i],
                        Profile = new Profile { ProfileString = profileString },
                        Material = new Material { MaterialString = materialString }
                    };
                    column.Insert();
                }

                // 모델 업데이트
                myModel.CommitChanges();


            }

            else {
                MessageBox.Show("연결 실패 ");
            
            }







        }
    }
}