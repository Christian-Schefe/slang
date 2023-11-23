let x = 5;
let y = 7;
print(1);
{
    print(2);
    let x = x + y;
    print(3);
    {
        print(4);
        let x = x + y;
        print(5);
        {
            print(6);
            let x = x + y;
            print(7, x, x * y, {let a = x - y; return a;});
        };
    };
};
