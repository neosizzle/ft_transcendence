import React, { useEffect, useState } from 'react';
import {
  BrowserRouter as Router,
  Routes,
  Route,
  Link
} from "react-router-dom";
import User from './User';
import Game from './Game';
import Chat from './Chat';

function App() {
  
  return (
    <Router>
        <div>
          <nav>
            <ul>
              <li>
                <Link to="/">Home</Link>
              </li>
              <li>
                <Link to="/chat">Chat</Link>
              </li>
              <li>
                <Link to="/users">Users</Link>
              </li>
              <li>
                <Link to="/game">Game</Link>
              </li>
            </ul>
          </nav>

          {/* A <Route> looks through its children <Route>s and
              renders the first one that matches the current URL. */}
          <Routes>
            <Route path="/chat" element = {<Chat/>}/>
            <Route path="/game" element = {<Game/>}/>
            <Route path="/users" element = {<User/>}/>
            <Route path="/" element = {<Home/>}/>
          </Routes>
        </div>
    </Router>
  );
}

function Home() {
  return <h2>Home</h2>;
}

export default App;
