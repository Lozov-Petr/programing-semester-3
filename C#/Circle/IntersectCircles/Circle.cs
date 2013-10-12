using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using GeomLib;

namespace IntersectCircles
{
    public class Circle
    {
        int X;
        int Y;
        int R;

        static Random rnd = new Random();

        public Circle(int X = 0, int Y = 0, int R = 0)
        {
            this.X = X;
            this.Y = Y;
            this.R = R;
        }

        public void createRandCircle()
        {
            this.R = rnd.Next(10, 65);
            this.X = rnd.Next(R, 200 - R);
            this.Y = rnd.Next(R, 200 - R);
        }

        public bool Intersect(Circle c)
        {
            return Geometry.TwoCircleIntersect(X, Y, R, c.X, c.Y, c.R);
        }

        public void Draw(Graphics graphics)
        {
            graphics.DrawEllipse(new Pen(Color.BlueViolet, 2.0f), X - R, Y - R, 2 * R, 2 * R);
        }
    }
}
