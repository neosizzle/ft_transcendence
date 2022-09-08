import React, { FunctionComponent } from "react";
import { useLocation } from "react-router-dom";
import { useAuth } from "../context/authContext";
import { ChatWidgetProvider } from "../context/chatWidgetContext";
import ChatWidget from "./ChatWidget";

const ChatWidgetWrapper: FunctionComponent = () => {
  const location = useLocation();
  const auth = useAuth();
  return location.pathname === "/chat" || !auth?.user ? null : (
    <ChatWidgetProvider>
      <ChatWidget />
    </ChatWidgetProvider>
  );
};

export default ChatWidgetWrapper;
