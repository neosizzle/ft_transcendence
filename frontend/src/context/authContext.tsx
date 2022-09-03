import { createContext, useContext, useState } from "react";
import React from "react";
import { API_ROOT, TOKEN_KEY, WS_ROOT } from "../constants";
import { auth_net_get, net_get } from "../utils";
import { io, Socket } from "socket.io-client";

// TODO HANDLE ERRORS

const authEndpoint = `${API_ROOT}/auth/authenticate`;
const userEndpoint = `${API_ROOT}/users/me`;
const logoutEndpoint = `${API_ROOT}/auth/logout`;
const chatWsEndpoint = `${WS_ROOT}/ws/chat`;
const gameWsEndpoint = `${WS_ROOT}/game`;
const userWsEndpoint = `${WS_ROOT}/ws/users`;

export interface User {
  id: number;
  email: string | null;
  avatar: string | null;
  status: string;
  level: number;
  username: string;
  intraID: string;
  intraName: string;
  createdAt: string;
  updatedAt: string;
  ranking: number;
  wins: number;
  losses: number;
}

interface Props {
  children: React.ReactNode;
}

export interface AuthCtx {
  // user : User | string | null,
  user: User | null;
  chatSocket: Socket | null;
  chatWidgetSocket: Socket | null;
  gameSocket: Socket | null;
  userSocket: Socket | null;
  login: (code?: string | null) => void;
  logout: () => void;
  reset: () => void;
}

const AuthContext = createContext<AuthCtx | null>(null);

export const AuthProvider = (props: Props) => {
  const [user, setUser] = useState<User | null>(null);
  const [chatSocket, setChatSocket] = useState<Socket | null>(null);
  const [chatWidgetSocket, setChatWidgetSocket] = useState<Socket | null>(null);
  const [gameSocket, setGameSocket] = useState<Socket | null>(null);
  const [userSocket, setUserSocket] = useState<Socket | null>(null);

  const login = async (code?: string | null) => {
    // alert(code) // uncomment this to obtain the code for backend testing. Usable only once.
    if (!code) {
      /* CODE DUPLICATION. TO DO: REFACTOR */
      const user = await auth_net_get(userEndpoint);

      //Init chat socket
      const chatSocketIo = io(chatWsEndpoint, {
        extraHeaders: {
          Authorization: `Bearer ${localStorage.getItem(TOKEN_KEY)}`,
        },
      });
      chatSocketIo.on("connection accepted", () => {
        chatSocketIo.emit("authHandshake");
      });
      setChatSocket(chatSocketIo);

      // Init chat widget socket
      const chatWidgetSocketIo = io(chatWsEndpoint, {
        extraHeaders: {
          Authorization: `Bearer ${localStorage.getItem(TOKEN_KEY)}`,
        },
      });
      chatWidgetSocketIo.on("connection accepted", () => {
        chatWidgetSocketIo.emit("authHandshake");
      });
      setChatWidgetSocket(chatWidgetSocketIo);

      // Init game socket
      const gameio = io(gameWsEndpoint, {
        extraHeaders: {
          Authorization: `Bearer ${localStorage.getItem(TOKEN_KEY)}`,
        },
      });
      setGameSocket(gameio);

      // Init user socket
      const usersSocketio = io(userWsEndpoint, {
        extraHeaders: {
          Authorization: `Bearer ${localStorage.getItem(TOKEN_KEY)}`,
        },
      });
      usersSocketio.on("connection accepted", () => {
        usersSocketio.emit("authHandshake");
      });

      setUserSocket(usersSocketio);

      setUser(user);
      return;
    }
    const data = await net_get(`${authEndpoint}/?code=${code}`);

    //Init chat socket
    const chatIo = io(chatWsEndpoint, {
      extraHeaders: {
        Authorization: `Bearer ${data.data.token}`,
      },
    });
    chatIo.on("connection accepted", () => {
      chatIo.emit("authHandshake");
    });
    setChatSocket(chatIo)

    // init chat widget socket
     const chatWidgetIo = io(chatWsEndpoint, {
      extraHeaders: {
        Authorization: `Bearer ${data.data.token}`,
      },
    });
    chatWidgetIo.on("connection accepted", () => {
      chatWidgetIo.emit("authHandshake");
    });
    setChatWidgetSocket(chatWidgetIo);

    // Init user socket
    const usersSocketio = io(userWsEndpoint, {
      extraHeaders: {
        Authorization: `Bearer ${data.data.token}`,
      },
    });
    usersSocketio.on("connection accepted", () => {
      usersSocketio.emit("authHandshake");
    });

    setUserSocket(usersSocketio);

    // set game socket
    const gameio = io(gameWsEndpoint, {
      extraHeaders: {
        Authorization: `Bearer ${data.data.token}`,
      },
    });
    setGameSocket(gameio);

    localStorage.setItem(TOKEN_KEY, data.data.token);
    const res = await auth_net_get(userEndpoint);
    setUser(res);
  };
  const logout = async () => {
    setUser(null);

    // Disconnect chat socket
    chatSocket?.disconnect();
    setChatSocket(null);
    
    // Disconnect chat widget socket
    chatWidgetSocket?.disconnect();
    setChatWidgetSocket(null);

    // Disconnect user socket
    userSocket?.disconnect();
    setUserSocket(null);

    // Disconnect game socket

    auth_net_get(logoutEndpoint);
    localStorage.removeItem(TOKEN_KEY);
  };
  const reset = async () => {
    await login();
  };

  return (
    <AuthContext.Provider
      value={{
        user,
        login,
        logout,
        reset,
        chatSocket,
        chatWidgetSocket,
        gameSocket,
        userSocket,
      }}
    >
      {props.children}
    </AuthContext.Provider>
  );
};

export const useAuth = () => {
  return useContext(AuthContext);
};
