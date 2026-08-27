package main
import "fmt"
type TextStyle struct{ Font string; Size int; Color string }
type StyleFactory struct{ styles map[string]*TextStyle }
func (f *StyleFactory) Get(font string,size int,color string)*TextStyle{if f.styles==nil{f.styles=map[string]*TextStyle{}};k:=fmt.Sprintf("%s|%d|%s",font,size,color);if s,ok:=f.styles[k];ok{return s};s:=&TextStyle{font,size,color};f.styles[k]=s;return s}
func main(){f:=StyleFactory{};r1:=f.Get("Inter",12,"red");r2:=f.Get("Inter",12,"red");_ = f.Get("Inter",12,"blue");fmt.Printf("styles=%d;shared=%t;text=ABC\n",len(f.styles),r1==r2)}
