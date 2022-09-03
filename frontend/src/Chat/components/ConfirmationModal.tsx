import React, { FunctionComponent, useState } from "react";
import { DEMOTE_ACTION, OUTGOING_DEMOTE, OUTGOING_OWNER_TRANSFER, OUTGOING_PROMOTE, OWNER_TRANSFER_ACTION, PROMOTE_ACTION } from "../../constants";
import { useAuth, User } from "../../context/authContext";
import { useChat } from "../../context/chatContext";
import { StringFunctionDict } from "../../utils";

const promptMessages: StringFunctionDict = {};
promptMessages[PROMOTE_ACTION] = (user: unknown) =>
  `You are about to promote ${(user as User).username} to admin`;
promptMessages[DEMOTE_ACTION] = (user: unknown) =>
`You are about to demote ${(user as User).username} from admin`;
promptMessages[OWNER_TRANSFER_ACTION] = (user: unknown) =>
`You are about to transfer ownership to ${(user as User).username}`;

const UsersIcon: FunctionComponent = () => {
  return (
    <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" className="mx-auto mb-4 text-gray-400 w-14 h-14 dark:text-gray-200">
    <path strokeLinecap="round" strokeLinejoin="round" d="M15 19.128a9.38 9.38 0 002.625.372 9.337 9.337 0 004.121-.952 4.125 4.125 0 00-7.533-2.493M15 19.128v-.003c0-1.113-.285-2.16-.786-3.07M15 19.128v.106A12.318 12.318 0 018.624 21c-2.331 0-4.512-.645-6.374-1.766l-.001-.109a6.375 6.375 0 0111.964-3.07M12 6.375a3.375 3.375 0 11-6.75 0 3.375 3.375 0 016.75 0zm8.25 2.25a2.625 2.625 0 11-5.25 0 2.625 2.625 0 015.25 0z" />
  </svg>
  
  );
};

interface ConfirmationModalProps {
  action: string;
  user: User;
  setOpenModal: (val : boolean) => void;
}

const ConfirmationModal: FunctionComponent<ConfirmationModalProps> = ({
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
				<UsersIcon />
			}
            <h3 className="mb-5 text-lg font-normal text-gray-500 dark:text-gray-400">
              {promptMessages[action](user)}
            </h3>

            <button
              onClick={() => {
                if (action === OWNER_TRANSFER_ACTION)
                {
                  // call ws transfer event
                  auth?.chatSocket?.emit(OUTGOING_OWNER_TRANSFER, JSON.stringify({dto : {ownerId : chat?.userToAdminAction?.id}, roomId : chat?.activeRoom?.id}))
                }
                else if (action === PROMOTE_ACTION)
                {
                  // call ws promtote event
                  auth?.chatSocket?.emit(OUTGOING_PROMOTE, JSON.stringify({roomId : chat?.activeRoom?.id, userId : chat?.userToAdminAction?.id}))
                }
                else if (action === DEMOTE_ACTION)
                {
                  // call ws demote event
                  const adminToDemote = chat?.admins.find(admin => admin.userId === chat.userToAdminAction?.id)
                  if (adminToDemote)
                    auth?.chatSocket?.emit(OUTGOING_DEMOTE, adminToDemote.id)
                }

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

export default ConfirmationModal;
