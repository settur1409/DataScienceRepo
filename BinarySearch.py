##Binary search
import sys
list = [2, 5, 7, 12, 14, 21, 28, 31, 36];

##print("list is sorted" if sorted(list) == list else "list not sorted");
if sorted(list) != list:
    print("list is not sorted exiting !!!!");
    sys.exit(0);

first_val = list[0];
found = 0;
last_val  = list[len(list)-1]
midindex = int(len(list)/2);
midValue = list[midindex];
toFind = 9;

while 1:
    if toFind < midValue:
        last_val = midValue;
        midValue = (first_val + last_val)/2;
    elif toFind > midValue:
        first_val = midValue;
        midValue = (first_val + last_val)/2;
    else:
        found = 1;
        break;
        
print('Value found' if found == 1 else 'Doesnt Found value');    
        
        