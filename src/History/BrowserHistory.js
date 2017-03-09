"use strict";

exports._state = function(Just) {
  return function(Nothing) {
    return function() {
      var state = window.history.state
      if (state === null)
        return Nothing
      else
        return Just(state)
    }
  }
}

exports._pushState = function(unit) {
  return function(state) {
    return function(name) {
      return function(url) {
        return function() {
          window.history.pushState(state, name, url)
          return unit
        }
      }
    }
  }
}

exports._replaceState = function(unit) {
  return function(state) {
    return function(name) {
      return function(url) {
        return function() {
          window.history.replaceState(state, name, url)
          return unit
        }
      }
    }
  }
}

exports._length = function() {
  return window.history.length
}

exports._go = function(unit) {
  return function(n) {
    return function () {
      window.history.go(n)
      return unit
    }
  }
}

exports._forward = function(unit) {
  return function() {
    window.history.forward()
    return unit
  }
}

exports._back = function(unit) {
  return function() {
    window.history.back()
    return unit
  }
}
