using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace CircleLib
{
    public class Circle
    {
        public int X;
        public int Y;
        public int R;

        public Circle(int X, int Y, int R)
        {
            this.X = X;
            this.Y = Y;
            this.R = R;
        }

        public bool Intersect(Circle c)
        {
            return Math.Sqrt(Math.Pow(this.X - c.X, 2) + Math.Pow(this.Y - c.Y, 2))
                <= this.R + c.R;
        }
    }
}
