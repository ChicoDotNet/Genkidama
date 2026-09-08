default rel

global _start

section .data
    message db "strategy=regular:100;vip:80;campaign:75;below:80", 10
    message_len equ $ - message

section .text

apply_strategy:
    ; edi = amount, rsi = interchangeable pricing routine
    call rsi
    ret

regular_strategy:
    mov eax, edi
    ret

vip_strategy:
    imul eax, edi, 80
    xor edx, edx
    mov ecx, 100
    div ecx
    ret

campaign_strategy:
    cmp edi, 100
    jb .unchanged
    imul eax, edi, 75
    xor edx, edx
    mov ecx, 100
    div ecx
    ret
.unchanged:
    mov eax, edi
    ret

_start:
    mov edi, 100
    lea rsi, [rel regular_strategy]
    call apply_strategy
    cmp eax, 100
    jne .fail

    mov edi, 100
    lea rsi, [rel vip_strategy]
    call apply_strategy
    cmp eax, 80
    jne .fail

    mov edi, 100
    lea rsi, [rel campaign_strategy]
    call apply_strategy
    cmp eax, 75
    jne .fail

    mov edi, 80
    lea rsi, [rel campaign_strategy]
    call apply_strategy
    cmp eax, 80
    jne .fail

    mov eax, 1
    mov edi, 1
    lea rsi, [rel message]
    mov edx, message_len
    syscall

    mov eax, 60
    xor edi, edi
    syscall

.fail:
    mov eax, 60
    mov edi, 1
    syscall
