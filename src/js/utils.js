binToRGB = function(bin){
    //var pbin = parseInt(bin,2);
    var pbin = bin;
    var r = pbin >> 16;
    var g = pbin >> 8 & 0xFF;
    var b = pbin & 0xFF;
    return [r,g,b];
}

// convert 0..255 R,G,B values to a hexidecimal color string
RGBToHex = function(r,g,b){
    var bin = r << 16 | g << 8 | b;
    return (function(h){
        return new Array(7-h.length).join("0")+h
    })(bin.toString(16).toUpperCase())
}

function BinToCSS (bin) {
    let [r, g, b] = binToRGB(bin);
    let hex = RGBToHex(r, g, b);
    return "#" + hex;
}

module.exports = {BinToCSS};


