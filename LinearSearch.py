##Linear search
list = [2, 5, 7, 12, 14, 21, 28, 31, 36];

position =0;
length = len(list)
found = 0;
userInput = input('Give me a value to find :: ');
toFind = int(userInput)
val =0;

for val in list:
    if val == toFind:
        found = 1;
        print("Found value ::"+ str(toFind));
        break;
    
print('Value found' if found == 1 else 'Doesnt Found value');