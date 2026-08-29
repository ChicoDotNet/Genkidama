global _start

section .data
    ; Bytecode for Expr := Number ('+' Number)*
    ; 1 <n> = number, 2 = plus, 0 = end
    program db 1, 2, 2, 1, 3, 2, 1, 4, 0
    output db 'interpreter=0', 10

section .text
_start:
    mov rsi, program
    xor eax, eax
    xor ebx, ebx

.next:
    mov dl, [rsi]
    inc rsi
    cmp dl, 0
    je .done
    cmp dl, 1
    je .number
    cmp dl, 2
    je .plus
    jmp .fail

.number:
    movzx ecx, byte [rsi]
    inc rsi
    add eax, ecx
    jmp .next

.plus:
    inc ebx
    jmp .next

.done:
    cmp eax, 9
    jne .fail
    cmp ebx, 2
    jne .fail
    add byte [output + 12], al
    mov eax, 1
    mov edi, 1
    mov rsi, output
    mov edx, 14
    syscall
    mov eax, 60
    xor edi, edi
    syscall

.fail:
    mov eax, 60
    mov edi, 1
    syscall
