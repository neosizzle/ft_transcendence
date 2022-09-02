import React, { FunctionComponent } from "react";
import { useLocation } from "react-router-dom";
import { ChatWidgetProvider } from "../context/chatWidgetContext";
import ChatWidget from "./ChatWidget";

const ChatWidgetWrapper: FunctionComponent = () => {
  const location = useLocation();
  return location.pathname === "/chat" ? null : (
    <ChatWidgetProvider>
      <ChatWidget />
    </ChatWidgetProvider>
  );
};

export default ChatWidgetWrapper;
