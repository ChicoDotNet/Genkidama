type TextStyle = { Font:string; Size:int; Color:string }
let styles = System.Collections.Generic.Dictionary<string, TextStyle>()
let getStyle font size color =
    let key = $"{font}|{size}|{color}"
    match styles.TryGetValue key with
    | true, value -> value
    | _ -> let value = { Font=font; Size=size; Color=color }; styles[key] <- value; value
let red1=getStyle "Inter" 12 "red"
let red2=getStyle "Inter" 12 "red"
let _blue=getStyle "Inter" 12 "blue"
printfn "styles=%d;shared=%s;text=ABC" styles.Count (if obj.ReferenceEquals(red1,red2) then "true" else "false")
