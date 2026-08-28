module PeerToPeerExample
open System.Collections.Generic

let run () =
    let inbox = ResizeArray<string>()
    let send fromPeer toPeer data =
        inbox.Add($"{fromPeer}>{toPeer}:{data}")
    send "peer-a" "peer-b" "block-42"
    send "peer-a" "peer-c" "block-42"
    String.concat ">" inbox = "peer-a>peer-b:block-42>peer-a>peer-c:block-42"
