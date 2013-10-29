using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace GeomLib
{
    public class Geometry
    {
        static public bool TwoCircleIntersect(int X1, int Y1, int R1, int X2, int Y2, int R2)
        {
            return Math.Sqrt(Math.Pow(X1 - X2 , 2) + Math.Pow(Y1 - Y2 , 2)) <= R1 + R2;
        }
    }
}
