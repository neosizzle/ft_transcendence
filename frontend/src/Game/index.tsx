import React from 'react';
import { Canvas } from './Canvas';


class Game extends React.Component {
	render() {
		return <Canvas width={400} height={300} style={{border: "1px solid black"}}/>;
	}
}

export default Game;
