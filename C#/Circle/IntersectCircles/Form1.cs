using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using CircleLib;

namespace Shell
{
    public partial class Form1 : Form
    {
        Random rnd = new Random();

        public Form1()
        {
            InitializeComponent();
        }

        private void button_Click(object sender, EventArgs e)
        {
            Circle c1 = CreateCircle();
            Circle c2 = CreateCircle();
            pictureBox.CreateGraphics().Clear(pictureBox.BackColor);
            DrawCircle(c1);
            DrawCircle(c2);
            if (c1.Intersect(c2)) label.Text = "Круги пересекаются";
            else label.Text = "Круги не пересекаются";
        }

        private Circle CreateCircle()
        {
            int R = rnd.Next(10, 65);
            return new Circle(rnd.Next(R, 200 - R), rnd.Next(R, 200 - R), R);
        }

        public void DrawCircle(Circle c)
        {
            pictureBox.CreateGraphics().DrawEllipse(new Pen(Color.Chocolate, 2), 
                c.X - c.R,c.Y - c.R, 2 * c.R, 2 * c.R);
        }
    }
}
