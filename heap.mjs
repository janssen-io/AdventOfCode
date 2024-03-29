function createMaxHeap(selector) {
    return new Heap(selector, (a,b) => a < b);
}

function createMinHeap(selector) {
    return new Heap(selector, (a,b) => a > b);
}

// Heap implementation taken from https://gist.github.com/tpae/54ec7371f947505967a2036b9c002428
function Heap(selector, cmp) {
    this.data = [];
    this.selector = selector;
    this.cmp = cmp;
    this.length = 0;
    this.size = 0;
}


Heap.prototype.push = function(...values) {
  for(const value of values) {
    this.data.push(value);
    this.bubbleUp(this.data.length-1);
    this.length = this.data.length;
    this.size = this.data.length;
  }
  
  // if (maxLength > 0 && this.data.length > maxLength) {
  //   this.data.pop();
  // }
};

Heap.prototype.bubbleUp = function(index) {
  while (index > 0) {
    // get the parent
    var parent = Math.floor((index + 1) / 2) - 1;

    // if parent is greater than child
    if (this.cmp(this.selector(this.data[parent]), this.selector(this.data[index]))) {
      // swap
      var temp = this.data[parent];
      this.data[parent] = this.data[index];
      this.data[index] = temp;
    }

    index = parent;
  }
};

Heap.prototype.shift = function() {
  if (this.data.length == 0) {
    return null;
  }
  this.length--; this.size--;

  var min = this.data[0];

  if (this.data.length == 1) {
      this.data = [];
  }
  else {
    // set first element to last element
    this.data[0] = this.data.pop();
    this.bubbleDown(0);
  }


  return min;
};

Heap.prototype.bubbleDown = function(index) {
  while (true) {
    var child = (index+1)*2;
    var sibling = child - 1;
    var toSwap = null;

    // console.log({l: this.data.length, index, child, sibling})

    // if current is greater (minheap) or smaller (maxheap) than child
    if (this.cmp(this.selector(this.data[index]), this.selector(this.data[child]))) {
      toSwap = child;
    }

    // if sibling is smaller than child, but also smaller than current
    if (this.cmp(this.selector(this.data[index]), this.selector(this.data[sibling]))
        && (this.data[child] == null ||
            (this.data[child] !== null
                && !this.cmp(this.selector(this.data[sibling]), this.selector(this.data[child]))))) {
        toSwap = sibling;
    }

    if (toSwap == null) {
      break;
    }

    var temp = this.data[toSwap];
    this.data[toSwap] = this.data[index];
    this.data[index] = temp;

    index = toSwap;
  }
};

Heap.prototype.peek = function() {
  return this.data[0];
}

Heap.prototype[Symbol.iterator] = function* () {
    let item = this.shift();
    while(item) {
      yield item;
      item = this.shift();
    }
};

export { Heap, createMinHeap, createMaxHeap }
