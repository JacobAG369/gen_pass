import Array "mo:base/Array";
import Char "mo:base/Char";
import Iter "mo:base/Iter";
import Nat "mo:base/Nat";
import Nat8 "mo:base/Nat8";
import Random "mo:base/Random";
import Text "mo:base/Text";
import Int "mo:base/Int";

actor PasswordGenerator {
    
    
    let mayusculas : [Char] = [
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
        'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'
    ];
    
    let minusculas : [Char] = [
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
        'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
    ];
    
    let numeros : [Char] = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
    
    let simbolos : [Char] = [
        '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '-', '_', '+',
        '=', '[', ']', '{', '}', ';', ':', ',', '.', '<', '>', '/', '?'
    ];
    
    
    private func randomInt(max : Nat) : async Nat {
        
        if (max == 0) {
            return 0;
        };
        
        let seed = await Random.blob();
        let random = Random.Finite(seed);
        
        
        let randomBytes = random.byte();
        switch (randomBytes) {
            case null { 0 };
            case (?byte) { 
                Nat8.toNat(byte) % max;
            };
        };
    };
    
    
    private func randomChar(chars : [Char]) : async Char {
        if (chars.size() == 0) {
            return ' '; 
        };
        
        let index = await randomInt(chars.size());
        
        let safeIndex = if (index >= chars.size()) { chars.size() - 1 } else { index };
        chars[safeIndex];
    };
    
    
    private func shuffleArray(arr : [Char]) : async [Char] {
        if (arr.size() <= 1) {
            return arr; 
        };
        
        let mutableArr = Array.thaw<Char>(arr);
        
        for (i in Iter.range(0, arr.size() - 1)) {
            let j = await randomInt(arr.size());
            if (i != j) {
                let temp = mutableArr[i];
                mutableArr[i] := mutableArr[j];
                mutableArr[j] := temp;
            };
        };
        
        Array.freeze<Char>(mutableArr);
    };
    
    // Función pública para generar contraseña
    public func generarContrasena(
        longitud : Nat,
        usarMayusculas : Bool,
        usarMinusculas : Bool,
        usarNumeros : Bool,
        usarSimbolos : Bool
    ) : async Text {
        
        
        if (not usarMayusculas and not usarMinusculas and not usarNumeros and not usarSimbolos) {
            return "Error: Selecciona al menos un tipo de caracteres";
        };
        
        if (longitud < 6) {
            return "Error: La longitud mínima recomendada es 6";
        };
        
        var caracteresDisponibles : [Char] = [];
        
        if (usarMayusculas) {
            caracteresDisponibles := Array.append(caracteresDisponibles, mayusculas);
        };
        
        if (usarMinusculas) {
            caracteresDisponibles := Array.append(caracteresDisponibles, minusculas);
        };
        
        if (usarNumeros) {
            caracteresDisponibles := Array.append(caracteresDisponibles, numeros);
        };
        
        if (usarSimbolos) {
            caracteresDisponibles := Array.append(caracteresDisponibles, simbolos);
        };
        
        
        if (caracteresDisponibles.size() == 0) {
            return "Error: No hay caracteres disponibles";
        };
        
        
        var passwordChars : [var Char] = Array.init<Char>(longitud, ' ');
        
        /
        for (i in Iter.range(0, longitud - 1)) {
            let randomIndex = await randomInt(caracteresDisponibles.size());
            // Asegurar índice dentro de límites
            let safeIndex = if (randomIndex >= caracteresDisponibles.size()) {
                caracteresDisponibles.size() - 1
            } else {
                randomIndex
            };
            passwordChars[i] := caracteresDisponibles[safeIndex];
        };
        
      
        var index : Nat = 0;
        
        if (usarMayusculas and index < longitud and mayusculas.size() > 0) {
            let char = await randomChar(mayusculas);
            passwordChars[index] := char;
            index += 1;
        };
        
        if (usarMinusculas and index < longitud and minusculas.size() > 0) {
            let char = await randomChar(minusculas);
            passwordChars[index] := char;
            index += 1;
        };
        
        if (usarNumeros and index < longitud and numeros.size() > 0) {
            let char = await randomChar(numeros);
            passwordChars[index] := char;
            index += 1;
        };
        
        if (usarSimbolos and index < longitud and simbolos.size() > 0) {
            let char = await randomChar(simbolos);
            passwordChars[index] := char;
            index += 1;
        };
        
       
        let shuffledChars = await shuffleArray(Array.freeze(passwordChars));
        
        Text.fromIter(Iter.fromArray(shuffledChars));
    };
    
    // FUncion para la fortaleza de la contraseña
    public func verificarFortaleza(password : Text) : async Text {
        let longitud = password.size();
        let chars = Iter.toArray(Text.toIter(password));
        
        var tieneMayuscula = false;
        var tieneMinuscula = false;
        var tieneNumero = false;
        var tieneSimbolo = false;
        
        for (char in Iter.fromArray(chars)) {
            if (Char.isUppercase(char)) tieneMayuscula := true;
            if (Char.isLowercase(char)) tieneMinuscula := true;
            if (Char.isDigit(char)) {
                tieneNumero := true;
            } else if (not Char.isAlphabetic(char)) {
                tieneSimbolo := true;
            };
        };
        
        var puntuacion = 0;
        
        if (longitud >= 8) puntuacion += 1;
        if (longitud >= 12) puntuacion += 1;
        if (longitud >= 16) puntuacion += 1;
        if (tieneMayuscula) puntuacion += 1;
        if (tieneMinuscula) puntuacion += 1;
        if (tieneNumero) puntuacion += 1;
        if (tieneSimbolo) puntuacion += 1;
        
        switch (puntuacion) {
            case 0 { "Débil" };
            case 1 { "Débil" };
            case 2 { "Débil" };
            case 3 { "Moderada" };
            case 4 { "Moderada" };
            case 5 { "Fuerte" };
            case 6 { "Fuerte" };
            case _ { "Muy Fuerte" };
        };
    };
}
