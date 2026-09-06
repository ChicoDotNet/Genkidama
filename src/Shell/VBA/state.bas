Option Explicit

Private Enum GateState
    GateLocked = 0
    GateUnlocked = 1
End Enum

Private Function Transition(ByVal currentState As GateState, ByVal action As String) As GateState
    Select Case currentState
        Case GateLocked
            If action = "coin" Then
                Transition = GateUnlocked
            Else
                Transition = GateLocked
            End If
        Case GateUnlocked
            If action = "push" Then
                Transition = GateLocked
            Else
                Transition = GateUnlocked
            End If
        Case Else
            Err.Raise vbObjectError + 513, "State", "Unknown gate state"
    End Select
End Function

Private Sub RequireState(ByVal condition As Boolean, ByVal message As String)
    If Not condition Then
        Err.Raise vbObjectError + 514, "State", message
    End If
End Sub

Public Sub VerifyStatePattern()
    Dim state As GateState
    state = GateLocked

    RequireState state = GateLocked, "initial state must be locked"

    state = Transition(state, "push")
    RequireState state = GateLocked, "push while locked must preserve state"

    state = Transition(state, "coin")
    RequireState state = GateUnlocked, "coin while locked must unlock"

    state = Transition(state, "coin")
    RequireState state = GateUnlocked, "duplicate coin must preserve unlocked state"

    state = Transition(state, "push")
    RequireState state = GateLocked, "push while unlocked must lock"

    Debug.Print "vba-state: passed"
End Sub
