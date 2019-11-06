// Type this in the terminal:
// node programming/javascript/hello.js

function sayHello(name) {
  console.log('Hello, ' + name);
}

sayHello("Andrés");

// typeof is an operator, not a function: we apply it to something by typing a space followed by the name of the thing we’d like to check the type of, e.g. typeof dress, as opposed to typeof(dress)

const a_number = 123.45;
console.log('the type of', a_number, 'is', typeof a_number);

console.log('the type of', console.log, 'is', typeof console.log); // functions are also a type of data!

const otherValues = [true, undefined, null];
for (let value of otherValues) {  // "let" creates a variable called value
  console.log('the type of', value, 'is', typeof value);
}

for (let value in otherValues) {  // "in" instead of "for" prints out the index
  console.log(value);             // indexing starts from 0 rather than 1!!
}




