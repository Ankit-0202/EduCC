/* sample.c */
int foo(int x, int y)
{
    int sum = x + y;
    return sum;
}

int main()
{
    int a = 5;
    int b = 7;
    int c = 0;

    c = foo(a, b);

    if (c > 10) {
        c++;
    } else {
        c--;
    }

    return c; // We'll return either 13 or 11
}
