@enum GateState locked unlocked
@enum GateAction coin push

function transition(state::GateState, action::GateAction)
    state == locked && action == coin && return unlocked
    state == unlocked && action == push && return locked
    state
end

function main()
    state = locked
    @assert state == locked

    state = transition(state, push)
    @assert state == locked

    state = transition(state, coin)
    @assert state == unlocked

    state = transition(state, coin)
    @assert state == unlocked

    state = transition(state, push)
    @assert state == locked

    println("julia-state: passed")
end

main()
