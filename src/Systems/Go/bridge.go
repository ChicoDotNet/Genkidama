package main

import "fmt"

type Device interface {
	PowerOn() string
	Mute() string
}

type TvDevice struct{}
func (TvDevice) PowerOn() string { return "TV:on" }
func (TvDevice) Mute() string { return "TV:muted" }

type RadioDevice struct{}
func (RadioDevice) PowerOn() string { return "Radio:on" }
func (RadioDevice) Mute() string { return "Radio:muted" }

type BasicRemote struct{ device Device }
func (r BasicRemote) Activate() string { return r.device.PowerOn() }

type MuteRemote struct{ device Device }
func (r MuteRemote) Activate() string { return r.device.Mute() }

func main() {
	tv := TvDevice{}
	radio := RadioDevice{}
	fmt.Println("basic-tv=" + BasicRemote{device: tv}.Activate())
	fmt.Println("basic-radio=" + BasicRemote{device: radio}.Activate())
	fmt.Println("mute-tv=" + MuteRemote{device: tv}.Activate())
	fmt.Println("mute-radio=" + MuteRemote{device: radio}.Activate())
}
