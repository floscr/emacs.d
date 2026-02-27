// [[file:.split-join-tag.js::*Split / Join JSX Tag Node][Split / Join JSX Tag Node:1]]
const useSpacesForTabs = true;
const tabSize = 4;
const sortOrder = [];
const closingBracketOnNewLine = true;

let input = process.argv[2]

// count the number of lines initally selected
let lineCount = input.split('\n').length
let lineText = input

// get the initial white space at the start of the line
let initialWhitespaceRegex = /\s*/i
let initialWhitespace = lineText.match(initialWhitespaceRegex)[0]

// get the opening tag
let openingTagRegex = /^[^\s]+/
let openingTag = input.match(openingTagRegex)[0]

// remove opening tag and trim
input = input.replace(openingTagRegex, '')
input = input.trim()

// get the ending bracket (if it's a "/>")
let endingBracket = ''
if (input.endsWith('/>')) {
    endingBracket = '/>'
}
else {
    endingBracket = '>'
}

// remove ending bracket and trim
if (endingBracket == '/>') {
    input = input.replace('/>', '')
}
else {
    input = input.substring(0, input.length - 1)
}
input = input.trim()

// create the indentation string
let indentationString
if (useSpacesForTabs == false) {
    indentationString = '\t'
}
else {
    indentationString = ' '.repeat(tabSize)
}

// regex to select all spaces that aren't within quotes
let spacesRegex = /\s+(?=([^"]*"[^"]*")*[^"]*$)/g

// get attributes into an array
let attributesString = input.replace(spacesRegex, '\n')
let attributesArray = attributesString.split('\n')

// sort the attributes array
let sortedAttributesArray = []
if (sortOrder.length) {

    // loop through sortOrder array
    sortOrder.forEach(search => {
        // loop through attributesArray
        let itemsToMove = []
        attributesArray.forEach((item, index) => {
            if (item.match(search)) {
                itemsToMove.push(index)
                // attributesArray.splice(index, 1)
            }
        })
        // move matched items from attributesArray to sortedAttributesArray (and sort them)
        let tempMatchedItems = []
        itemsToMove.forEach(indexItem => {
            tempMatchedItems.push(attributesArray[indexItem])
        })
        tempMatchedItems.sort()
        sortedAttributesArray.push(...tempMatchedItems)

        // remove matched items from attributesArray
        for (var i = itemsToMove.length - 1; i >= 0; --i) {
            attributesArray.splice(itemsToMove[i], 1)
        }
    })

    // sort remaining attributes and add to sortedAttributesArray
    attributesArray.sort()
    sortedAttributesArray.push(...attributesArray)
}
else {
    sortedAttributesArray = attributesArray
}

// add the opening tag
let result = openingTag

// set the join character based on number of lines initially selected
// (newLine if one line, space if more)
let joinCharacter = lineCount > 1 ? ' ' : '\n'

// if there are no attributes, set joinCharacter to ''
if (sortedAttributesArray.length == 1 && sortedAttributesArray[0] == '') {
    joinCharacter = ''
}

// add the sorted attributes to the textSplit string
if (lineCount > 1) {
    sortedAttributesArray.forEach(item => {
        result += joinCharacter + item
    })
}
else {
    sortedAttributesArray.forEach(item => {
        result += joinCharacter + initialWhitespace + indentationString + item
    })
}

// configure ending bracket (new line or not new line)
if (lineCount > 1) {
    if (endingBracket == '/>') {
        endingBracket = ' ' + endingBracket
    }
}
else {
    if (closingBracketOnNewLine) {
        endingBracket = '\n' + initialWhitespace + endingBracket
    }
    else if (endingBracket == '/>') {
        endingBracket = ' ' + endingBracket
    }
}

// add the ending bracket
result = result + endingBracket

console.log(result)
// Split / Join JSX Tag Node:1 ends here
