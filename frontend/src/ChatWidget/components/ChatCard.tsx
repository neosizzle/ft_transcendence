import React, { FunctionComponent } from "react";
import { Message } from "../../Chat/classes";

interface ChatCardProps {
  message: Message;
  floatRight?: boolean;
  system?: boolean;
}

const ChatCard: FunctionComponent<ChatCardProps> = ({
  message,
  floatRight,
  system,
}) => {
  return (
    <div
      className={`py-5 ${
        floatRight ? "text-right" : system ? "text-center" : "text-left"
      }`}
    >
      {" "}
      {floatRight ? (
        message.message
      ) : system ? (
        <span className="text-sm text-gray-400">{message.message}</span>
      ) : (
        `${message.user?.username} : ${message.message}`
      )}
    </div>
  );
};

export default ChatCard;
