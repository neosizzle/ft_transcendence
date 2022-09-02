import React, { FunctionComponent, useState } from "react";
import { BAN_ACTION, MUTE_ACTION, OUTGOING_JOIN } from "../../constants";
import { useAuth, User } from "../../context/authContext";
import { useChat } from "../../context/chatContext";
import { StringFunctionDict } from "../../utils";

const promptMessages: StringFunctionDict = {};
promptMessages[MUTE_ACTION] = (user: unknown) =>
  `Select mute time for ${(user as User).username} (THIS ACTION IS NOT REVERSIBLE) `;
promptMessages[BAN_ACTION] = (user: unknown) =>
  `Select ban time for ${(user as User).username} (THIS ACTION IS NOT REVERSIBLE) `;

const MuteIcon: FunctionComponent = () => {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      fill="none"
      viewBox="0 0 24 24"
      strokeWidth={1.5}
      stroke="currentColor"
      className="mx-auto mb-4 text-gray-400 w-14 h-14 dark:text-gray-200"
    >
      <path
        strokeLinecap="round"
        strokeLinejoin="round"
        d="M17.25 9.75L19.5 12m0 0l2.25 2.25M19.5 12l2.25-2.25M19.5 12l-2.25 2.25m-10.5-6l4.72-4.72a.75.75 0 011.28.531V19.94a.75.75 0 01-1.28.53l-4.72-4.72H4.51c-.88 0-1.704-.506-1.938-1.354A9.01 9.01 0 012.25 12c0-.83.112-1.633.322-2.395C2.806 8.757 3.63 8.25 4.51 8.25H6.75z"
      />
    </svg>
  );
};

const BanIcon: FunctionComponent = () => {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      fill="none"
      viewBox="0 0 24 24"
      strokeWidth={1.5}
      stroke="currentColor"
      className="mx-auto mb-4 text-gray-400 w-14 h-14 dark:text-gray-200"
    >
      <path
        strokeLinecap="round"
        strokeLinejoin="round"
        d="M18.364 18.364A9 9 0 005.636 5.636m12.728 12.728A9 9 0 015.636 5.636m12.728 12.728L5.636 5.636"
      />
    </svg>
  );
};

interface TimeSelectionModalProps {
  action: string;
  user: User;
  setOpenModal: (val : boolean) => void;
}

const TimeSelectionModal: FunctionComponent<TimeSelectionModalProps> = ({
  setOpenModal,
  action,
  user,
}) => {
  const [roomIdInput, setRoomIdInput] = useState<string>("");
  const [passwordInput, setPasswordInput] = useState<string>("");
  const auth = useAuth();
  const chat = useChat();

  return (
    <div className="fixed top-0 left-0 right-0 z-50 h-full overflow-x-hidden overflow-y-auto md:inset-0  bg-gray-300/50">
      <div className="relative w-full p-4 flex justify-center item-center">
        <div className="relative bg-white rounded-lg shadow dark:bg-gray-700">
          <button
            onClick={() => {
              setOpenModal(false);
            }}
            type="button"
            className="absolute top-3 right-2.5 text-gray-400 bg-transparent hover:bg-gray-200 hover:text-gray-900 rounded-lg text-sm p-1.5 ml-auto inline-flex items-center dark:hover:bg-gray-800 dark:hover:text-white"
            data-modal-toggle="popup-modal"
          >
            <svg
              className="w-5 h-5"
              fill="currentColor"
              viewBox="0 0 20 20"
              xmlns="http://www.w3.org/2000/svg"
            >
              <path
                fillRule="evenodd"
                d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z"
                clipRule="evenodd"
              ></path>
            </svg>
          </button>
          <div className="p-6 text-center">
            {
				action == MUTE_ACTION ? 
				<MuteIcon /> :
				<BanIcon />
			}
            <h3 className="mb-5 text-lg font-normal text-gray-500 dark:text-gray-400">
              {promptMessages[action](user)}
            </h3>

            {/* Inputs*/}
            {/* <input
              type="number"
              value={roomIdInput}
              onChange={(e) => setRoomIdInput(e.target.value)}
              className="mb-2 bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-gray-500 focus:border-gray-500 block w-full p-2.5  dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-gray-500 dark:focus:border-gray-500"
              placeholder="Room id"
            />

            <input
              type="password"
              value={passwordInput}
              onChange={(e) => setPasswordInput(e.target.value)}
              className="mb-2 bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-gray-500 focus:border-gray-500 block w-full p-2.5  dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-gray-500 dark:focus:border-gray-500"
              placeholder="Room password (Optional)"
            /> */}

            <button
              onClick={() => {

                // close window
                setOpenModal(false);
              }}
              type="button"
              className={`w-full text-gray-500 bg-white hover:bg-gray-100 focus:ring-4 focus:outline-none focus:ring-gray-200 rounded-lg border border-gray-200 text-sm font-medium px-5 py-2.5 hover:text-gray-900 focus:z-10 dark:bg-gray-700 dark:text-gray-300 dark:border-gray-500 dark:hover:text-white dark:hover:bg-gray-600 dark:focus:ring-gray-600`}
            >
              OK
            </button>
            <button
              onClick={() => {
                // if (loading) return;
                setOpenModal(false);
              }}
              type="button"
              className={`w-full mt-2 text-gray-500 bg-white hover:bg-gray-100 focus:outline-none focus:ring-gray-200 rounded-lg border-gray-200 text-sm font-medium px-5 py-2.5 hover:text-gray-900 focus:z-10 dark:bg-gray-700`}
            >
              Cancel
            </button>
          </div>
        </div>
      </div>
    </div>
  );
};

export default TimeSelectionModal;
