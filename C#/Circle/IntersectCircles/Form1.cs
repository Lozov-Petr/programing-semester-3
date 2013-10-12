using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;


namespace IntersectCircles
{
    public partial class Form1 : Form
    {
        Circle c1 = new Circle();
        Circle c2 = new Circle();

        public Form1()
        {
            InitializeComponent();
        }

        private void button_Click(object sender, EventArgs e)
        {
            c1.createRandCircle();
            c2.createRandCircle();

            pictureBox.Refresh();

            if (c1.Intersect(c2)) label.Text = "Круги пересекаются";
            else label.Text = "Круги не пересекаются";
        }

        private void pictureBox_Paint(object sender, PaintEventArgs e)
        {
            e.Graphics.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.HighQuality;
            e.Graphics.Clear(pictureBox.BackColor);
            c1.Draw(e.Graphics);
            c2.Draw(e.Graphics);
        }
    }
}