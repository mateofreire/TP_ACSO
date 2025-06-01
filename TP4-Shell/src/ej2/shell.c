#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>

#define MAX_COMMANDS 256 // Soporta hasta 256 procesos en el pipeline
#define MAX_TOKENS    64
#define MAX_ARGS      (MAX_TOKENS + 1)
#define CMD_BUF_SIZE 4096 // Capacidad de línea extendida a 4096

static int count_arguments(const char *s) {
    bool in_quote = false;
    bool in_tok = false;
    int count = 0;

    const char *p = s;

    while (*p) {
        if (*p == '"') {
            if (!in_quote) {
                in_quote = true;
                if (!in_tok) { in_tok = true; count++; }
            } else {
                in_quote = false;
            }
        } else if (isspace((unsigned char)*p) && !in_quote) {
            if (in_tok) {
                in_tok = false;
            }
        } else {
            if (!in_tok) { in_tok = true; count++; }
        }
        p++;
    }
    return count;
}

void split_arguments(char *input, char **args, int *arg_count) {
    char *p = input;
    *arg_count = 0;

    while (*p) {
        while (*p && isspace((unsigned char)*p)) { *p = '\0'; p++; }
        if (!*p) { break; }

        if (*p == '"') {
            p++;
            args[*arg_count] = p;
            (*arg_count)++;
            while (*p && *p != '"') p++;
            if (*p == '"') { *p = '\0'; p++; }
        } else {
            args[*arg_count] = p;
            (*arg_count)++;
            while (*p && !isspace((unsigned char)*p)) p++;
            if (*p) { *p = '\0'; p++; }
        }

        if (*arg_count >= MAX_TOKENS) break;
    }
    args[*arg_count] = NULL;
}

int main() {
    char command[CMD_BUF_SIZE];
    char *commands[MAX_COMMANDS];
    int command_count;

    while (1) {
        if (isatty(STDIN_FILENO)) {
            printf("Shell> ");
            fflush(stdout);
        }

        if (!fgets(command, sizeof(command), stdin)) {
            break;  // EOF
        }

        // Eliminar salto de línea final y trim
        command[strcspn(command, "\n")] = '\0';
        char *start = command;
        while (*start && isspace((unsigned char)*start)) start++;
        char *end = start + strlen(start) - 1;
        while (end > start && isspace((unsigned char)*end)) { *end = '\0'; end--; }
        memmove(command, start, strlen(start) + 1);

        // Verificar comillas balanceadas
        int quote_count = 0;
        for (int i = 0; command[i]; i++) {
            if (command[i] == '"') quote_count++;
        }

        if (quote_count % 2 != 0) {
            fprintf(stderr, "Syntax error: comillas abiertas sin cerrar\n");
            continue;
        }

        // Salir si se escribe "exit"
        if (strcmp(command, "exit") == 0) break;

        // Ignorar líneas vacías
        if (command[0] == '\0') continue;

        // Verificar tuberías mal ubicadas
        size_t len = strlen(command);
        if (command[0] == '|' || command[len - 1] == '|') {
            fprintf(stderr, "Syntax error: tubería al inicio o al final\n");
            continue;
        }

        if (strstr(command, "||")) {
            fprintf(stderr, "Syntax error: '||' no permitido\n");
            continue;
        }

        // Verificar que no haya comandos vacíos entre tuberías
        bool empty_between = false;
        char *p = command;
        while ((p = strchr(p, '|')) != NULL) {
            char *q = p + 1;
            while (*q && isspace((unsigned char)*q)) q++;
            if (*q == '|') { empty_between = true; break; }
            p = q;
        }
        if (empty_between) {
            fprintf(stderr, "Syntax error: comando vacío entre tuberías\n");
            continue;
        }

        // Dividir la línea en comandos separados por '|'
        command_count = 0;
        char *tok = strtok(command, "|");
        while (tok && command_count < MAX_COMMANDS) {
            commands[command_count] = tok;
            command_count++;
            tok = strtok(NULL, "|");
        }
        if (command_count == 0) continue;

        // Verificar que ningún comando tenga más de MAX_TOKENS argumentos
        bool too_many_args = false;
        for (int i = 0; i < command_count; i++) {
            int actual = count_arguments(commands[i]);
            if (actual > MAX_TOKENS) {
                fprintf(stderr, "Syntax error: demasiados argumentos\n");
                too_many_args = true;
                break;
            }
        }
        if (too_many_args) continue;

        // Crear los pipes necesarios para conectar los comandos
        int pipes[MAX_COMMANDS - 1][2];
        for (int i = 0; i < command_count - 1; i++) {
            if (pipe(pipes[i]) < 0) {
                perror("pipe");
                exit(1);
            }
        }

        // Crear procesos hijos con fork y ejecutar los comandos
        for (int i = 0; i < command_count; i++) {
            pid_t pid = fork();
            if (pid < 0) {
                perror("fork");
                exit(1);
            }
            if (pid == 0) {
                // Redireccionar entrada desde pipe si no es el primer comando
                if (i > 0) {
                    dup2(pipes[i - 1][0], STDIN_FILENO);
                }

                // Redireccionar salida al siguiente comando si no es el último
                if (i < command_count - 1) {
                    dup2(pipes[i][1], STDOUT_FILENO);
                }

                // Cerrar todos los extremos de pipes en el hijo
                for (int j = 0; j < command_count - 1; j++) {
                    close(pipes[j][0]);
                    close(pipes[j][1]);
                }

                // Separar argumentos del comando actual
                char *args[MAX_ARGS];
                int arg_count = 0;
                split_arguments(commands[i], args, &arg_count);

                // Ejecutar el comando
                execvp(args[0], args);

                // Si exec falla, mostrar error y salir
                perror("execvp");
                exit(1);
            }
        }

        // Cerrar todos los extremos de pipes en el padre
        for (int i = 0; i < command_count - 1; i++) {
            close(pipes[i][0]);
            close(pipes[i][1]);
        }

        // Esperar a que terminen todos los hijos
        for (int i = 0; i < command_count; i++) {
            wait(NULL);
        }
    }
    return 0;
}