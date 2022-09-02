import React, { useEffect, useState } from "react";
import { BrowserRouter as Router, Routes, Route } from "react-router-dom";
import User from "./User";
import Game from "./Game";
import Chat from "./Chat";
import { AuthProvider } from "./context/authContext";
import Login from "./Login";
import Protected from "./commonComponents/Protected";
import Logout from "./Logout";
import Navbar from "./commonComponents/navbar";
import Home from "./Home";
import { ChatProvider } from "./context/chatContext";
import ChatWidgetWrapper from "./ChatWidget";

function App() {
  const [initLoad, setInitLoad] = useState<number>(0);

  // initial loading
  useEffect(() => {
    setInitLoad(1);
    // setTimeout(() => setInitLoad(1), 1000)
  }, []);

  return !initLoad ? (
    <div>Loading..</div>
  ) : (
    <AuthProvider>
      <Router>
        <div>
          <Navbar />
          {/* A <Route> looks through its children <Route>s and
              renders the first one that matches the current URL. */}
          <Routes>
            <Route
              path="/chat"
              element={
                <Protected>
                  <ChatProvider><Chat/></ChatProvider>
                </Protected>
              }
            />
            <Route path="/game"
              element={
                <Protected>
                  <Game />
                </Protected>
              }
            />
            <Route
              path="/users/*"
              element={
                <Protected>
                  <User />
                </Protected>
              }
            />
            <Route
              path="/logout"
              element={
                <Protected>
                  <Logout />
                </Protected>
              }
            />
            <Route path="/login" element={<Login />} />
            <Route path="/" element={<Home />} />
          </Routes>
          <ChatWidgetWrapper />
        </div>
      </Router>
    </AuthProvider>
  );
}

export default App;
