import React, { Component } from 'react';
import Menu from "./pages/menu/menu";
import Layer1 from "./pages/home/layer-1/layer1";
import Layer2 from "./pages/home/layer-2/layer2";
import './App.scss';

class App extends Component {
  render() {
    return (
      <div className="App">
        <Menu />
        <div className="app-container">
          <Layer1 />
          <Layer2 />
        </div>
      </div>
    );
  }
}

export default App;
