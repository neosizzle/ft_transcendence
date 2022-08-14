import { createContext, useContext, useEffect, useState } from "react";
import React from "react";
import { API_ROOT, TOKEN_KEY, WS_ROOT } from "../constants";
import { auth_net_get, net_get } from "../utils";
import { io, Socket } from "socket.io-client";

// TODO HANDLE ERRORS

const authEndpoint = `${API_ROOT}/auth/authenticate`;
const userEndpoint = `${API_ROOT}/users/me`;
const logoutEndpoint = `${API_ROOT}/auth/logout`;
const chatWsEndpoint = `${WS_ROOT}/ws/chat`;

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
}

interface Props {
  children: React.ReactNode;
}

interface AuthCtx {
  // user : User | string | null,
  user: User | null;
  chatSocket: Socket | null;
  login: (code?: string | null) => void;
  logout: () => void;
  reset: () => void;
}

const AuthContext = createContext<AuthCtx | null>(null);

export const AuthProvider = (props: Props) => {
  // const [user, setUser] = useState<User | string | null>(null);
  const [user, setUser] = useState<User | null>(null);
  const [chatSocket, setChatSocket] = useState<Socket | null>(null);

  const login = async (code?: string | null) => {
    // alert(code) // uncomment this to obtain the code for backend testing. Usable only once.
    if (!code) {
      const user = await auth_net_get(userEndpoint);
      setUser(user);
      return;
    }
    const data = await net_get(`${authEndpoint}/?code=${code}`);
    const socketio = io(chatWsEndpoint, {
      extraHeaders: {
        Authorization: `Bearer ${data.data.token}`,
      },
    });
    socketio.on("connection accepted", () => {
      socketio.emit("authHandshake");
    });
    setChatSocket(socketio);
    localStorage.setItem(TOKEN_KEY, data.data.token);
    const res = await auth_net_get(userEndpoint);
    setUser(res);
  };
  const logout = async () => {
    setUser(null);
    chatSocket?.disconnect();
    setChatSocket(null);
    auth_net_get(logoutEndpoint);
    localStorage.removeItem(TOKEN_KEY);
  };
  const reset = async () => {
    await login();
  };

  return (
    <AuthContext.Provider value={{ user, login, logout, reset, chatSocket }}>
      {props.children}
    </AuthContext.Provider>
  );
};

export const useAuth = () => {
  return useContext(AuthContext);
};
