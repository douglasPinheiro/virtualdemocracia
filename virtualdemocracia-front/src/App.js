import React, { Component } from 'react';
import Menu from "./components/menu/menu";
import './App.scss';

class App extends Component {
  render() {
    return (
      <div className="App">
        <Menu />
        <header className="App-header">
        </header>
      </div>
    );
  }
}

export default App;
