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
import { AuthProvider, useAuth } from './context/authContext';
import Login from './Login';
import Protected from './commonComponents/Protected';


const Navbar = () => {
  const auth = useAuth();
  return (
      <nav>
        {/* {auth && auth.user? auth.user : "no user"} */}
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
              {
                auth?.user?.length &&  auth?.user?.length > 0?
                null
                :
                <li><Link to="/login">Login</Link></li>
              }
            </ul>
      </nav>
  )
}

function App() {

  return (
    <AuthProvider>
      <Router>
        <div>
          
          <Navbar/>
          
          {/* A <Route> looks through its children <Route>s and
              renders the first one that matches the current URL. */}
          <Routes>
            <Route path="/chat" element = {<Chat/>}/>
            <Route path="/game" element = {<Game/>}/>
            <Route path="/users" element = {<Protected><User/></Protected>}/>
            <Route path="/login" element = {<Login/>}/>
            <Route path="/" element = {<Home/>}/>
          </Routes>
        </div>
    </Router>
    </AuthProvider>
  );
}

function Home() {
  return <h2>Home</h2>;
}

export default App;
