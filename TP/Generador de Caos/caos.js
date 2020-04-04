let quantity = Math.floor(Math.random() * (100 - 50 + 1)) + 50
var big_data = [];
var collatz_array = [];

function collatz(num){
    let collatz_list = [num];
    while(num != 1){
      if(num % 2 == 0){
        num = parseInt(num / 2); 
      }else{
        num = (num * 3) + 1;
      }
      collatz_list.push(num);
    }
    return collatz_list;
}

for(let i = 0; i<quantity; i++){
    let index_x = Math.floor(Math.random() * (6 - 1 + 1)) + 1
    let index_y = Math.floor(Math.random() * (6 - 2 + 1)) + 2
    let matrix = [];
    let num_collatz = (Math.floor(Math.random() * (50 - 5 + 1)) + 5);
    var collatz_sequence = collatz(num_collatz)
    var count = 0;
    var index_rnd_x = Math.floor(Math.random() * (index_x - 1 ));
    var index_rnd_y = Math.floor(Math.random() * (index_y - 1 ));
    var set_dir = Math.floor(Math.random() * 2); //1 para izquierda, 0 para abajo
    var random_pos_coll = Math.floor(Math.random() * ((collatz_sequence.length-1) - 1 + 1) + 1)
    var camino_random = [];

    var object = {
        'collatz_num' : num_collatz,
        'collatz_sequence' : collatz_sequence
    }
    collatz_array.push(object);

    for(let x = 0; x<index_x; x++){

        matrix[x] = [];

        for(let y = 0; y<index_y;y++){

            if(index_rnd_x == x){
                if(index_rnd_y == y){
                    if(count < collatz_sequence.length){
                        matrix[x][y] = collatz_sequence[count];
                        if(set_dir == 1){
                            index_rnd_y++;
                            count ++;
                            set_dir = Math.floor(Math.random() * 2);
                        }else{
                            index_rnd_x++;
                            count++;
                            set_dir = Math.floor(Math.random() * 2);
                        }
                    }else{
                        let numero = Math.floor(Math.random() * (50 - 1 + 1)) + 1
                        if(numero % 5 == 0){
                            matrix[x][y] = collatz_sequence[random_pos_coll];
                            random_pos_coll = Math.floor(Math.random() * ((collatz_sequence.length-1) - 1 + 1) + 1)
                        }else {
                            matrix[x][y] = numero;
                        }              
                    }
                } else{
                    let numero = Math.floor(Math.random() * (50 - 1 + 1)) + 1
                    if(numero % 5 == 0){
                        matrix[x][y] = collatz_sequence[random_pos_coll];
                        random_pos_coll = Math.floor(Math.random() * ((collatz_sequence.length-1) - 1 + 1) + 1)
                    }else {
                        matrix[x][y] = numero;
                    }
                }
            }else{
                let numero = Math.floor(Math.random() * (50 - 1 + 1)) + 1
                if(numero % 5 == 0){
                    matrix[x][y] = collatz_sequence[random_pos_coll];
                    random_pos_coll = Math.floor(Math.random() * ((collatz_sequence.length-1) - 1 + 1) + 1)
                }else {
                }
                matrix[x][y] = numero;
            }

        }

    }
    big_data.push(matrix)
}

big_data.forEach((element, index) => {
    let max_number = 0;
    let print_file = `sopa${index+6} :: Tablero
    <br>
    sopa${index+6} = [`;
    element.forEach((data, index) => {
        print_file += "["+data+"]";
        data.forEach(yuno => {
            if(yuno >= max_number){
                max_number = yuno;
            }
        })
        if(index != element.length - 1){
            print_file += ",";
        }
    })
    print_file += `] 
    <br><br>`;
    document.getElementById("caos").innerHTML += print_file
});

collatz_array.forEach((element,index) => {
    let data = `
    <strong>Sopa n° ${index+6}</strong>
    <br>
    <strong>Collatz_num</strong> = ${element.collatz_num}
    <br>
    <strong>mayorSecuenciaDeCollatz sopa${index+6} ${element.collatz_num}</strong>
    <br>
    <strong>mayorSecuenciaDeCollatzPermutando sopa${index+6} ${element.collatz_num}</strong>
    <br>
    <strong>Collatz_sequence</strong><br>[${element.collatz_sequence}]
    <br>
    <br>
    `;
    document.getElementById("queso").innerHTML += data;
})