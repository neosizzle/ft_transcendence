import React, { useEffect, useState } from 'react';
//, { useEffect, useState }
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
    <nav className='bg-gray-800'>
          <div className="max-w-7xl mx-auto px-2 sm:px-6 lg:px-8">
            <div className="relative flex items-center justify-between h-16">
              <div className="flex-shrink-0 flex items-center">
                {/* Img can be changed */}
                <img className="block h-8 w-auto" src="https://tailwindui.com/img/logos/workflow-logo-indigo-500-mark-white-text.svg" alt="Workflow" />
              </div>
              <div className="sm:block sm:ml-6">
                <div className="flex space-x-4">
                  <Link to="/" className="bg-gray-900 text-white px-3 py-2 rounded-md text-sm font-medium" aria-current="page">Home</Link>
                  {
                    auth?.user?.length &&  auth?.user?.length > 0?
                    null
                    :
                    <Link to="/login" className="text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md text-sm font-medium">Login</Link>
                  }
                  <Link to="/chat" className="text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md text-sm font-medium">Chat</Link>
                  <Link to="/users" className="text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md text-sm font-medium">Users</Link>
                  <Link to="/game" className="text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md text-sm font-medium">Game</Link>
                </div>
              </div>
            </div>
          </div>
        </nav>
  )
}

function App() {

  const [initLoad, setInitLoad] = useState<number>(0);

  // initial loading
  useEffect(() => {
    setTimeout(() => setInitLoad(1), 1000)
  }, [])
  
  return (
    !initLoad ? 
    <div>
      asdf
    </div> : 
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
