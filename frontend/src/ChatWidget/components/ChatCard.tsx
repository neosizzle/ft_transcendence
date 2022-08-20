import React, { FunctionComponent } from "react";
import { Message } from "../../Chat/classes";
import { useAuth } from "../../context/authContext";

const INV_STR = "/invite";

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
  const auth = useAuth();
  const isInvite = message.message.startsWith(INV_STR);

  return (
    <div
      className={`py-5 ${
        system || isInvite ? "text-center" : floatRight ? "text-right" : "text-left"
      }`}
    >
      {" "}
      {isInvite ? (
        <span className="text-sm text-gray-400">
          Invite for {auth?.user?.id}
        </span>
      ) : floatRight ? (
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
