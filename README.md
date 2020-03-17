# Single Layer Neural Network

The first part of the program defines a vector type using linked lists. This is the simplest method for implementing vectors, although not efficient. A list of vectors is used to define a matrix type. To perform matrix multiplication, the second matrix is transposed and dot products are performed for all combinations of vectors. No parallelism is implemented which could otherwise greatly increase performance.

The second part of the program reads a CSV file and splits it in half for testing and training data. Both features and labels are stored as matrices instead of plain vectors for simplicity. The program uses Data.Text.IO to process the text file, which contains functions for splitting text and reading the file itself.

The neural network defines a weight matrix that is used when mapping over test samples. A new weight matrix is defined after each epoch, with the final weight matrix used for testing. The initial weight matrix is constructed with an infinite list of numbers from randoms gen :: [Double]. The program takes rows*columns from the list and shortens the list as values are moved into each vector.

The program was run with a small subset of the MNIST data set (100 samples). Using this configuration, 72% accuracy was achieved. When the entire data set was used, the program terminated with “unable to commit 1048576 bytes of memory”. It is not clear why GHC was not able to commit a single megabyte of memory, but more practical implementations of neural networks would place more emphasis on efficiency.
