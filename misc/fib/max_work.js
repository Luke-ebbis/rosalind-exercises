#! /usr/bin/env node
/**Script written by Max Bakker
 *
 * This is a script written by Max Bakker (@MaxBaktBrood) to give me an example
 * of a java script programme. This script was written by him on the 27th of
 * January, 2022. 
 *
 * Currently the script holds solutions in javascript for the rosalind
 * exercises up until and including the gc content calculation -- S.
**/

const { start } = require('repl');

/**
 * @type { Array<string> }
 * invoer van de gebruiker
 */
let invoer = process.argv;

/** @type { string? } */
let arg = null;

if (invoer[2].startsWith('-')) {
    arg = invoer[2].split('-').join('');
    invoer.splice(2, 1)
}
invoer = invoer.splice(2);

function dnaString(i) {
    return (i.join('dnaString(invoer)').toUpperCase())
}

function help() {
    return (`USAGE: ${process.argv[0]} ${process.argv[1]} <parm> <input>
    
    parms:

    -*              This function
    NONE:           opdracht 1: Counting Nucliotides
    -r:             opdracht 2: DNA to RNA
    -cs:            opdracht 3: complementing a strand
    -f:             opdracht 4: Those fucking rabbits
    -cg             opdracht 5: CG-content
    
    `)
}
function err(i) {
    if (i) {console.log(i)};
    process.exit();
}

function opdracht1(invoer) {
/** @type { int } Voorkomendheid van de desbetreffende letter*/
let 
a = invoer.split('A').length - 1,
c = invoer.split('C').length - 1,
t = invoer.split('T').length - 1,
g = invoer.split('G').length - 1;

return (console.log(
    `${a} ${c} ${g} ${t}`
));
}

function opdracht2(invoer) {
    let rna = invoer.split('T').join('U');
    return (rna);
}

function opdracht3(invoer) {
    invoer = invoer.split('').reverse();
    invoer = invoer.map(i => {
        switch (i) {
            case 'A':
                return 'T';
            case 'T':
                return 'A';
            case 'C':
                return 'G';
            case 'G':
                return 'C';
        }
    });
    return (invoer.join(''));
}

function opdracht4(invoer) {
    
    let n, r;

    switch (invoer.length) {
        case 0:
            return err('Te weinig informatie ingevoerd.');
        case 1:
            n = invoer[0];
            r = 1;
            break;
        default:
            n = invoer[0];
            r = invoer[1];
            break;
    };

    let populatie = [1, 0];


    while (n > 1) {
                // console.log(`
            // populatie maand ${populatie.length - 1}:  ${populatie[0]} (${Math.round(100 * ((populatie[0] - populatie[1]) / populatie[1]))}% toename)
        // `);
        n--;

        let nieuwePopulatie = (populatie[0]) + (populatie[1] * r);

        populatie.splice(0, 0, nieuwePopulatie);


    };
    console.log(`
            populatie maand ${populatie.length - 1}:  ${populatie[0]} (${Math.round(100 * ((populatie[0] - populatie[1]) / populatie[1]))}% toename)
    `)

    return (populatie[0]);
};

async function opdracht5() {

    const leesInterface = require('readline').createInterface({
        input:process.stdin,
        output:process.stdout
    });

    let cg_gehaltes = [];
    let x = 1;

    do {
        let eiwitreeks = await new Promise(resolve => {
            leesInterface.question(`Voer eiwitreeks ${x} in:\n`, resolve)
          })
        
        //let eiwitreeks = invoer.value;
        let cg_aantal = eiwitreeks.split('C').join('G').split('G').length - 1;
        let cg_gehalte = 100 * (cg_aantal / eiwitreeks.split('').length);

        cg_gehaltes.push({
            cg_gehalte:cg_gehalte,
            eiwitreeks:eiwitreeks
        })

        x++;
    } while (x <= 10);


    cg_gehaltes.sort(function(a, b){return (a.cg_gehalte - b.cg_gehalte)}); 

    return(
        `
        ${cg_gehaltes[0].eiwitreeks}\n
        ${cg_gehaltes[0].cg_gehalte}
        `
        
        );

}

/** @type { string? } */
let uitkomst = null;

async function opdrachten(arg) {
    switch (arg) {
        case null:
            uitkomst = opdracht1(dnaString(invoer));
            break;
    
        case 'r':
            uitkomst = opdracht2(dnaString(invoer));
            break;
        
        case 'cs':
            uitkomst = opdracht3(dnaString(invoer));
            break;
    
        case 'f':
            uitkomst = opdracht4(invoer);
            break;
    
        case 'cg':
            uitkomst = await opdracht5();
            break;
    
        default:
            uitkomst = help();
            break;
    }
    
    console.log(uitkomst);
}

opdrachten(arg).then(function () {
    return;
});
return;


