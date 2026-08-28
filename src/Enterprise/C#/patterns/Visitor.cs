using System;
using System.Linq;
namespace Genkidama.PatternExamples;
public static class VisitorExample { private interface IShape{} private sealed record Circle(double R):IShape; private sealed record Rect(double W,double H):IShape; private static double Area(IShape s)=>s switch{Circle c=>Math.PI*c.R*c.R,Rect r=>r.W*r.H,_=>0}; public static bool Run()=>Math.Abs(new IShape[]{new Circle(2),new Rect(3,4)}.Sum(Area)-(4*Math.PI+12))<1e-9; }
