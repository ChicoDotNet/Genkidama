section .data
    postgres_connect_msg db "Connecting to PostgreSQL", 0
    postgres_query_msg db "Querying PostgreSQL", 0
    mysql_connect_msg db "Connecting to MySQL", 0
    mysql_query_msg db "Querying MySQL", 0

section .bss

section .text
    global _start

_start:
    ; Example usage
    mov rdi, postgres_connect_msg
    call print
    mov rdi, postgres_query_msg
    call print

    mov rdi, mysql_connect_msg
    call print
    mov rdi, mysql_query_msg
    call print

    ; Exit
    mov rax, 60
    xor rdi, rdi
    syscall

print:
    mov rax, 1
    mov rdi, 1
    mov rsi, rdi
    mov rdx, [rsi-1] ; calculate string length
    syscall
    ret
