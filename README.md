# axcc

A small C11 compiler written in C++.

This is just my personal work and it's not an optimizing compiler. Each stage of compilation in axcc is clearly divided and easy to read, including scanner, preprocessor (Dave Prosser's C macro expanding algorithm), evaluator, and parser (Basic recursive descent + ad-hoc tricks). Besides, axcc strictly follows the C11 standard, like clang with "-pedantic".

So far I have completed the frontend and the internal ast can be printed. The backend is not finished so I haven't written a main function yet. If you need one you can write it yourself because it would be really simple (Just look at the test code in the tests directory).



# TODO

Code Generator.

