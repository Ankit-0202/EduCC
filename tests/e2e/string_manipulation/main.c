#include <stdio.h>
#include <string.h>

int main() {
    char str1[20] = "Hello";
    char str2[20] = "World";
    strcat(str1, " ");
    strcat(str1, str2);
    printf("%s\n", str1);
    return 0;
}
